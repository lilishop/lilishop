package cn.lili.modules.wechat.serviceimpl;

import cn.hutool.crypto.digest.DigestUtil;
import cn.lili.common.enums.ClientTypeEnum;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.connect.entity.Connect;
import cn.lili.modules.connect.service.ConnectService;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.lili.modules.wechat.entity.dos.WechatOrderSync;
import cn.lili.modules.wechat.entity.dos.WechatOrderSyncLog;
import cn.lili.modules.wechat.service.WechatOrderSyncJournal;
import cn.lili.modules.wechat.support.WechatOrderShippingBuilder;
import cn.lili.modules.wechat.support.WechatOrderSyncException;
import cn.lili.modules.wechat.util.WechatAccessTokenUtil;
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import com.alibaba.fastjson2.JSONWriter;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.*;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * 验证上报开关、人工权限、支付锁、结果记录及同版本重试，不连接微信或业务数据库。
 *
 * @author Bulbasaur
 * @since 2026-11-04
 */
class WechatMPServiceImplOrderSwitchTest {
    private WechatMPServiceImpl service;
    private SettingService settings;
    private OrderService orders;
    private WechatAccessTokenUtil tokens;
    private WechatOrderSyncJournal journal;
    private WechatOrderShippingBuilder builder;
    private RLock lock;
    private Order order;
    private WechatOrderSync sync;
    private WechatOrderSyncLog log;

    /** 初始化纯内存依赖，HTTP 方法默认只返回明确成功响应。 */
    @BeforeEach
    void setUp() throws Exception {
        service = spy(new WechatMPServiceImpl());
        settings = mock(SettingService.class);
        orders = mock(OrderService.class);
        tokens = mock(WechatAccessTokenUtil.class);
        journal = mock(WechatOrderSyncJournal.class);
        builder = mock(WechatOrderShippingBuilder.class);
        RedissonClient redisson = mock(RedissonClient.class);
        lock = mock(RLock.class);
        when(redisson.getLock(anyString())).thenReturn(lock);
        when(lock.tryLock(0, -1, TimeUnit.SECONDS)).thenReturn(true);
        when(lock.isHeldByCurrentThread()).thenReturn(true);
        ConnectService connects = mock(ConnectService.class);
        Connect connect = new Connect();
        connect.setUnionId("payer-openid");
        when(connects.queryConnect(any())).thenReturn(connect);
        ReflectionTestUtils.setField(service, "settingService", settings);
        ReflectionTestUtils.setField(service, "orderService", orders);
        ReflectionTestUtils.setField(service, "wechatAccessTokenUtil", tokens);
        ReflectionTestUtils.setField(service, "journal", journal);
        ReflectionTestUtils.setField(service, "shippingBuilder", builder);
        ReflectionTestUtils.setField(service, "redissonClient", redisson);
        ReflectionTestUtils.setField(service, "connectService", connects);
        order = new Order();
        order.setSn("order-1"); order.setReceivableNo("wx-transaction-1"); order.setMemberId("member-1");
        order.setClientType("WECHAT_MP"); order.setPaymentMethod("WECHAT"); order.setPayStatus("PAID");
        order.setOrderStatus("TAKE"); order.setOrderType("VIRTUAL");
        when(orders.getBySn("order-1")).thenReturn(order);
        when(orders.list(any(com.baomidou.mybatisplus.core.conditions.Wrapper.class))).thenReturn(List.of(order));
        sync = new WechatOrderSync(); sync.setId("state-1"); sync.setTransactionId(order.getReceivableNo()); sync.setStatus("PENDING");
        when(journal.ensure(order)).thenReturn(sync);
        log = new WechatOrderSyncLog(); log.setOperation("SHIPPING");
        when(journal.begin(any(), any(), anyString(), any())).thenAnswer(inv -> { log.setOperation(inv.getArgument(2)); return log; });
        when(builder.build(anyList(), any())).thenAnswer(inv -> payload());
        configure(null);
        Setting payment = new Setting(); payment.setSettingValue("{\"mpAppId\":\"app-1\",\"mchId\":\"mch-1\"}");
        when(settings.get(SettingEnum.WECHAT_PAYMENT.name())).thenReturn(payment);
        doReturn(JSON.parseObject("{\"errcode\":0,\"is_trade_managed\":true}")).when(service).doPostWithJson(anyString(), anyMap());
        doCallRealMethod().when(journal).requireManager(any());
    }

    /** 关闭自动开关不调用微信，但保存待上报原因且释放支付锁。 */
    @Test
    void disabledRecordsPendingWithoutCallingWechat() {
        configure("{\"wechatOrderAutoUploadEnabled\":false}");
        service.uploadShippingInfo("order-1");
        verify(service, never()).doPostWithJson(anyString(), anyMap());
        verify(journal).finish(sync, log, "PENDING", "AUTO_DISABLED", "自动上报开关已关闭，可按需人工补报");
        verify(lock).unlock();
    }

    /** 缺失、空、旧版及显式开启配置均保留自动上报能力。 */
    @Test
    void missingAndLegacySettingsPreserveExistingBehavior() {
        for (String value : new String[]{null, "", "{}", "null", "{\"wechatOrderAutoUploadEnabled\":null}", "{\"wechatOrderAutoUploadEnabled\":true}"}) {
            configure(value);
            service.uploadShippingInfo("order-1");
        }
        verify(service, times(6)).doPostWithJson(eq("wxa/sec/order/upload_shipping_info"), anyMap());
    }

    /** 非小程序或非微信支付订单不能创建同步记录。 */
    @Test
    void excludesOtherChannelsAndPayments() {
        order.setClientType("PC"); service.uploadShippingInfo("order-1");
        order.setClientType("WECHAT_MP"); order.setPaymentMethod("WALLET"); service.uploadShippingInfo("order-1");
        verifyNoInteractions(journal);
        verify(service, never()).doPostWithJson(anyString(), anyMap());
    }

    /** 支付流水缺失保留失败原因，不用空流水调用微信。 */
    @Test
    void missingTransactionIsRecorded() {
        order.setReceivableNo(""); service.uploadShippingInfo("order-1");
        verify(journal).finish(eq(sync), eq(log), eq("FAILED"), eq("MISSING_TRANSACTION"), anyString());
        verify(service, never()).doPostWithJson(anyString(), anyMap());
    }

    /** 同版本成功事件只审计为跳过，不重复提交。 */
    @Test
    void duplicateSuccessSkipsUpload() {
        Map<String,Object> request = payload();
        request.put("order_key", Map.of("order_number_type",2,"transaction_id",order.getReceivableNo()));
        request.put("payer", Map.of("openid","payer-openid"));
        sync.setStatus("SUCCESS");
        sync.setRequestHash(DigestUtil.sha256Hex(JSON.toJSONString(request, JSONWriter.Feature.MapSortField)));
        sync.setSuccessHash(sync.getRequestHash());
        service.uploadShippingInfo("order-1");
        // 暂停事件可以产生待处理结果，但不能抹掉已经成功的版本依据。
        sync.setStatus("PENDING");
        service.uploadShippingInfo("order-1");
        verify(service, never()).doPostWithJson(eq("wxa/sec/order/upload_shipping_info"), anyMap());
        verify(journal, times(2)).finish(sync, log, "SKIPPED", null, null);
    }

    /** 接口超时后相同数据重试保留原上传时间，避免使用重试时间改写原发货版本。 */
    @Test
    void retryReusesOriginalUploadTimeAfterUnknownResult() {
        doThrow(new RuntimeException("timeout")).when(service).doPostWithJson(eq("wxa/sec/order/upload_shipping_info"), anyMap());
        service.uploadShippingInfo("order-1");
        String original = sync.getUploadTime();
        assertNotNull(original);
        service.uploadShippingInfo("order-1");
        assertEquals(original, sync.getUploadTime());
        verify(journal, times(2)).finish(eq(sync), eq(log), eq("FAILED"), eq("REQUEST_OR_STORAGE_ERROR"), anyString());
    }

    /** 持锁中的同笔支付禁止并发调用，也不会释放别人的锁。 */
    @Test
    void busyPaymentDoesNotCreateAttempt() throws Exception {
        when(lock.tryLock(0, -1, TimeUnit.SECONDS)).thenReturn(false);
        assertThrows(ServiceException.class, () -> service.uploadShippingInfo("order-1"));
        verifyNoInteractions(journal);
        verify(lock, never()).unlock();
    }

    /** 允许管理员在自动开关关闭时人工补报；记录人工身份。 */
    @Test
    void manualRetryIsIndependentOfAutoSwitch() {
        configure("{\"wechatOrderAutoUploadEnabled\":false}");
        AuthUser admin = AuthUser.builder().id("admin-1").username("运营").role(UserEnums.MANAGER).build();
        try (MockedStatic<UserContext> context = mockStatic(UserContext.class)) {
            context.when(UserContext::getCurrentUser).thenReturn(admin);
            assertEquals("SUCCESS", service.retryShippingInfo("order-1"));
        }
        verify(journal).begin(sync, order, "SHIPPING", admin);
    }

    /** 服务层再次验证身份，商家不能通过人工接口补报其他店铺订单。 */
    @Test
    void manualRetryRejectsStoreUser() {
        AuthUser seller = AuthUser.builder().role(UserEnums.STORE).build();
        try (MockedStatic<UserContext> context = mockStatic(UserContext.class)) {
            context.when(UserContext::getCurrentUser).thenReturn(seller);
            assertThrows(ServiceException.class, () -> service.retryShippingInfo("order-1"));
        }
        verify(orders, never()).getBySn(anyString());
    }

    /** 外部业务错误保存微信错误码，日志不泄露手机号和长凭据。 */
    @Test
    void rejectionIsSanitizedAndRecorded() {
        doThrow(new WechatOrderSyncException("1001", "openid=abcdefghijklmnop12345678 mobile=13812345678", false))
                .when(service).doPostWithJson(eq("wxa/sec/order/upload_shipping_info"), anyMap());
        service.uploadShippingInfo("order-1");
        verify(journal).finish(eq(sync), eq(log), eq("FAILED"), eq("1001"), argThat(s -> !s.contains("13812345678") && !s.contains("abcdefghijklmnop")));
    }

    /** 收货通知必须等待全部子订单完成，且不把本地已发货等同于微信已受理。 */
    @Test
    void receiveWaitsForWholePaymentCompletion() {
        sync.setStatus("SUCCESS"); service.notifyConfirmReceive("order-1");
        verify(service, never()).doPostWithJson(eq("wxa/sec/order/notify_confirm_receive"), anyMap());
        verify(journal).finish(eq(sync), eq(log), eq("PENDING"), eq("WAIT_COMPLETE"), anyString());
    }

    /** 令牌只刷新一次，每次重新传入新令牌而非重复拼接 URL。 */
    @Test
    void tokenExpiryRefreshesOnce() {
        doCallRealMethod().when(service).doPostWithJson(anyString(), anyMap());
        when(tokens.cgiAccessToken(ClientTypeEnum.WECHAT_MP)).thenReturn("old", "new");
        doReturn(JSON.parseObject("{\"errcode\":40001}")).when(service).postJson(anyString(), anyMap(), eq("old"));
        doReturn(JSON.parseObject("{\"errcode\":0}")).when(service).postJson(anyString(), anyMap(), eq("new"));
        assertEquals(0, service.doPostWithJson("test/path", Map.of()).getIntValue("errcode"));
        verify(tokens).removeAccessToken(ClientTypeEnum.WECHAT_MP);
        verify(service).postJson(eq("test/path"), anyMap(), eq("new"));
    }

    /** 两次令牌均失效时停止重试，避免递归循环。 */
    @Test
    void repeatedTokenExpiryStopsAfterTwoAttempts() {
        doCallRealMethod().when(service).doPostWithJson(anyString(), anyMap());
        when(tokens.cgiAccessToken(ClientTypeEnum.WECHAT_MP)).thenReturn("bad");
        doReturn(JSON.parseObject("{\"errcode\":40001}")).when(service).postJson(anyString(), anyMap(), anyString());
        assertThrows(WechatOrderSyncException.class, () -> service.doPostWithJson("test/path", Map.of()));
        verify(service, times(2)).postJson(eq("test/path"), anyMap(), eq("bad"));
    }

    /** 生成一笔虚拟交付的稳定请求片段。 */
    private Map<String,Object> payload() {
        Map<String,Object> value = new LinkedHashMap<>();
        value.put("logistics_type",3); value.put("delivery_mode",1);
        value.put("shipping_list",List.of(Map.of("item_desc","虚拟商品")));
        return value;
    }

    /** 设置自动开关配置；null 模拟尚未保存配置。 */
    private void configure(String value) {
        Setting setting = null;
        if (value != null) { setting = new Setting(); setting.setSettingValue(value); }
        when(settings.get(SettingEnum.ORDER_SETTING.name())).thenReturn(setting);
    }
}
