package cn.lili.modules.wechat.support;

import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.vo.OrderSimpleVO;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.wechat.entity.dos.WechatOrderSync;
import cn.lili.modules.wechat.entity.dos.WechatOrderSyncLog;
import cn.lili.modules.wechat.entity.vo.WechatOrderVO;
import cn.lili.modules.wechat.mapper.WechatOrderSyncMapper;
import cn.lili.modules.wechat.mapper.WechatOrderSyncLogMapper;
import cn.lili.modules.wechat.service.WechatOrderSyncJournal;
import com.alibaba.fastjson2.JSON;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Date;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * 验证日志结果对发货状态的隔离、历史待核查及合并支付的批量展示，不执行数据库迁移。
 *
 * @author Bulbasaur
 * @since 2026-11-09
 */
class WechatOrderSyncJournalTest {
    private WechatOrderSyncJournal journal;
    private WechatOrderSyncMapper states;
    private OrderService orders;

    /** 初始化 Lambda 字段元数据，不启动 Spring 或业务数据库。 */
    @BeforeAll
    static void metadata() {
        for (Class<?> type : List.of(Order.class, WechatOrderSync.class)) {
            TableInfoHelper.initTableInfo(new MapperBuilderAssistant(new MybatisConfiguration(), "test"), type);
        }
    }

    /** 使用内存 Mapper，便于检查发货和收货两类结果的更新边界。 */
    @BeforeEach
    void setUp() {
        journal = new WechatOrderSyncJournal();
        states = mock(WechatOrderSyncMapper.class);
        orders = mock(OrderService.class);
        ReflectionTestUtils.setField(journal, "syncMapper", states);
        ReflectionTestUtils.setField(journal, "logMapper", mock(WechatOrderSyncLogMapper.class));
        ReflectionTestUtils.setField(journal, "orderService", orders);
    }

    /** 收货通知失败不能回退已经成功的发货同步状态。 */
    @Test
    void receiveFailureDoesNotOverwriteShippingState() {
        WechatOrderSync sync = new WechatOrderSync(); sync.setId("state-1"); sync.setStatus("SUCCESS");
        journal.finish(sync, log("RECEIVE"), "FAILED", "REMOTE_ERROR", "通知失败");
        ArgumentCaptor<LambdaUpdateWrapper> update = ArgumentCaptor.forClass(LambdaUpdateWrapper.class);
        verify(states).update(isNull(), update.capture());
        assertFalse(update.getValue().getSqlSet().contains("status"));
        assertFalse(update.getValue().getSqlSet().contains("success_hash"));
    }

    /** 跳过成功版本不能用最近一次失败版本摘要替换成功依据或改写原成功时间。 */
    @Test
    void skipPreservesSuccessfulVersionAndTime() {
        Date original = new Date(1000);
        WechatOrderSync sync = new WechatOrderSync(); sync.setId("state-1");
        sync.setSuccessTime(original); sync.setRequestHash("last-failed"); sync.setSuccessHash("last-success");
        journal.finish(sync, log("SHIPPING"), "SKIPPED", null, null);
        ArgumentCaptor<LambdaUpdateWrapper> update = ArgumentCaptor.forClass(LambdaUpdateWrapper.class);
        verify(states).update(isNull(), update.capture());
        assertFalse(update.getValue().getSqlSet().contains("success_hash"));
        assertTrue(update.getValue().getParamNameValuePairs().containsValue(original));
    }

    /** 合并支付子订单共用失败结果，当前页只进行一次状态批量查询。 */
    @Test
    void mergedChildrenShareOneState() {
        Order a = order("A"), b = order("B");
        when(orders.list(any(Wrapper.class))).thenReturn(List.of(a,b));
        WechatOrderSync sync = new WechatOrderSync(); sync.setTransactionId("wx-1"); sync.setStatus("FAILED"); sync.setErrorMessage("缺少物流");
        when(states.selectList(any(Wrapper.class))).thenReturn(List.of(sync));
        Page<OrderSimpleVO> page = page("A","B");
        IPage<WechatOrderVO> result = journal.fill(page);
        for (WechatOrderVO row : result.getRecords()) { assertEquals("FAILED",row.getWechatSyncStatus()); assertEquals("缺少物流",row.getWechatSyncError()); }
        verify(states).selectList(any(Wrapper.class));
    }

    /** 历史无本地记录不推断成功，即使商城订单已经发货。 */
    @Test
    void missingRecordRemainsUnchecked() {
        when(orders.list(any(Wrapper.class))).thenReturn(List.of(order("A")));
        when(states.selectList(any(Wrapper.class))).thenReturn(List.of());
        Page<OrderSimpleVO> page = page("A");
        IPage<WechatOrderVO> result = journal.fill(page);
        assertEquals("UNCHECKED",result.getRecords().get(0).getWechatSyncStatus());
        assertNull(result.getRecords().get(0).getWechatSyncTime());
    }

    /** 专用 VO 保留分页、基础字段及商品展示，同时不修改来源分页或通用接口字段。 */
    @Test
    void dedicatedViewPreservesPageAndOrderContract() {
        when(orders.list(any(Wrapper.class))).thenReturn(List.of(order("A")));
        when(states.selectList(any(Wrapper.class))).thenReturn(List.of());
        Page<OrderSimpleVO> source = page("A");
        source.setCurrent(3); source.setSize(20); source.setTotal(123);
        OrderSimpleVO original = source.getRecords().get(0);
        original.setFlowPrice(88.0); original.setOrderType("NORMAL"); original.setOrderStatus("DELIVERED");
        original.setPayStatus("PAID"); original.setOrderPromotionType("NORMAL"); original.setDeliverStatus("DELIVERED");
        original.setGroupGoodsId("goods-1"); original.setGroupName("测试商品"); original.setGroupNum("2");
        IPage<WechatOrderVO> result = journal.fill(source);
        WechatOrderVO view = result.getRecords().get(0);
        assertNotSame(source, result); assertNotSame(original, view);
        assertEquals(3, result.getCurrent()); assertEquals(20, result.getSize()); assertEquals(123, result.getTotal());
        assertEquals("A", view.getSn()); assertEquals(88.0, view.getFlowPrice());
        assertEquals("测试商品", view.getOrderItems().get(0).getName());
        assertEquals("UNCHECKED", view.getWechatSyncStatus());
        assertTrue(JSON.toJSONString(view).contains("wechatSyncStatus"));
        assertFalse(JSON.toJSONString(original).contains("wechatSync"));
        assertSame(original, source.getRecords().get(0));
    }

    /** 空分页也返回专用分页结构，保留筛选后的总数及页码，不发起额外查询。 */
    @Test
    void emptyPageKeepsMetadataWithoutQueryingOrders() {
        Page<OrderSimpleVO> source = new Page<>(4,20,0);
        IPage<WechatOrderVO> result = journal.fill(source);
        assertEquals(4,result.getCurrent()); assertEquals(20,result.getSize()); assertEquals(0,result.getTotal());
        assertTrue(result.getRecords().isEmpty());
        verifyNoInteractions(states, orders);
    }

    /** 构造带起始时间的待结束日志。 */
    private WechatOrderSyncLog log(String operation) {
        WechatOrderSyncLog log = new WechatOrderSyncLog(); log.setId("log-1"); log.setOperation(operation); log.setCreateTime(new Date()); return log;
    }

    /** 构造共享微信支付流水的商城子订单。 */
    private Order order(String sn) {
        Order order = new Order(); order.setSn(sn); order.setReceivableNo("wx-1"); return order;
    }

    /** 构造实际展示顺序的订单分页。 */
    private Page<OrderSimpleVO> page(String... sns) {
        Page<OrderSimpleVO> page = new Page<>(1,20);
        page.setRecords(java.util.Arrays.stream(sns).map(sn -> { OrderSimpleVO row = new OrderSimpleVO(); row.setSn(sn); return row; }).toList());
        return page;
    }
}
