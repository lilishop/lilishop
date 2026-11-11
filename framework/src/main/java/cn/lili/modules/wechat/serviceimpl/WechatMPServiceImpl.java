package cn.lili.modules.wechat.serviceimpl;

import cn.hutool.crypto.digest.DigestUtil;
import cn.hutool.http.HttpRequest;
import cn.hutool.http.HttpResponse;
import cn.lili.common.enums.ClientTypeEnum;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.modules.connect.entity.Connect;
import cn.lili.modules.connect.entity.enums.SourceEnum;
import cn.lili.modules.connect.service.ConnectService;
import cn.lili.modules.member.entity.dto.ConnectQueryDTO;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.OrderSetting;
import cn.lili.modules.system.entity.dto.payment.WechatPaymentSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.lili.modules.wechat.entity.dos.WechatOrderSync;
import cn.lili.modules.wechat.entity.dos.WechatOrderSyncLog;
import cn.lili.modules.wechat.service.WechatMPService;
import cn.lili.modules.wechat.service.WechatOrderSyncJournal;
import cn.lili.modules.wechat.support.WechatOrderShippingBuilder;
import cn.lili.modules.wechat.support.WechatOrderSyncException;
import cn.lili.modules.wechat.util.WechatAccessTokenUtil;
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import com.alibaba.fastjson2.JSONWriter;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.TimeUnit;

/**
 * 微信小程序业务实现；按支付流水归集全部子订单，自动上报和人工补报复用校验、锁及日志。
 * 外部请求与数据库事务分离；相同成功快照不重复提交，失败重试复用同版本上传时间。
 *
 * @author Bulbasaur
 */
@Service
public class WechatMPServiceImpl implements WechatMPService {
    @Autowired
    private OrderService orderService;
    @Autowired
    private ConnectService connectService;
    @Autowired
    private WechatAccessTokenUtil wechatAccessTokenUtil;
    @Autowired
    private SettingService settingService;
    @Autowired
    private RedissonClient redissonClient;
    @Autowired
    private WechatOrderSyncJournal journal;
    @Autowired
    private WechatOrderShippingBuilder shippingBuilder;

    /** 自动事件执行发货归集；非小程序微信支付订单直接跳过，不创建同步记录。 */
    @Override
    @Transactional(propagation = Propagation.NOT_SUPPORTED)
    public void uploadShippingInfo(String orderSn) {
        execute(orderSn, "SHIPPING", null);
    }

    /** 人工补报独立于自动上报开关，但必须满足平台管理员身份、支付及实际履约条件。 */
    @Override
    @Transactional(propagation = Propagation.NOT_SUPPORTED)
    public String retryShippingInfo(String orderSn) {
        AuthUser operator = UserContext.getCurrentUser();
        journal.requireManager(operator);
        return execute(orderSn, "SHIPPING", operator);
    }

    /** 仅整笔支付全部完成且发货已成功时通知收货；不会覆盖发货同步状态。 */
    @Override
    @Transactional(propagation = Propagation.NOT_SUPPORTED)
    public void notifyConfirmReceive(String orderSn) {
        execute(orderSn, "RECEIVE", null);
    }

    /**
     * 串行化同笔支付的全部操作；锁使用看门狗续期，数据库仅在日志方法内开启短事务。
     * @return SUCCESS/SKIPPED/PENDING/FAILED；手工调用方应刷新列表及日志展示具体原因
     */
    private String execute(String orderSn, String operation, AuthUser operator) {
        Order order = orderService.getBySn(orderSn);
        if (order == null) throw new ServiceException("订单不存在");
        if (!eligible(order)) {
            if (operator != null) throw new ServiceException("仅可补报微信小程序来源且使用微信支付的订单");
            return "SKIPPED";
        }
        String key = WechatOrderSyncJournal.transactionKey(order);
        RLock lock = redissonClient.getLock("wechat:order:sync:" + DigestUtil.sha256Hex(key));
        boolean locked = false;
        try {
            locked = lock.tryLock(0, -1, TimeUnit.SECONDS);
            if (!locked) throw new ServiceException("该笔支付正在同步，请稍后刷新重试");
            order = orderService.getBySn(orderSn);
            if (order == null || !eligible(order) || !key.equals(WechatOrderSyncJournal.transactionKey(order))) {
                throw new ServiceException("订单支付信息已变化，请刷新后重试");
            }
            WechatOrderSync sync = journal.ensure(order);
            WechatOrderSyncLog log = journal.begin(sync, order, operation, operator);
            String outcome = "SUCCESS";
            String errorCode = null;
            String message = null;
            try {
                if (operator == null && !isOrderAutoUploadEnabled()) throw pending("AUTO_DISABLED", "自动上报开关已关闭，可按需人工补报");
                if (blank(order.getReceivableNo())) throw failure("MISSING_TRANSACTION", "微信支付交易号缺失，请核查支付流水");
                List<Order> group = paymentGroup(order);
                WechatPaymentSetting payment = paymentSetting();
                if ((!blank(sync.getAppId()) && !sync.getAppId().equals(payment.getMpAppId()))
                        || (!blank(sync.getMchId()) && !sync.getMchId().equals(payment.getMchId()))) {
                    throw failure("PAYMENT_CONFIG_CHANGED", "当前小程序或商户号与同步快照不一致，请核查历史支付配置");
                }
                if (!doPostWithJson("wxa/sec/order/is_trade_managed", Map.of("appid", payment.getMpAppId())).getBooleanValue("is_trade_managed")) {
                    throw pending("NOT_MANAGED", "微信侧尚未开通发货信息管理");
                }
                if ("SHIPPING".equals(operation)) {
                    Map<String, String> codes = new HashMap<>();
                    Map<String, Object> request = shippingBuilder.build(group,
                            name -> deliveryCode(name, codes));
                    Connect connect = connectService.queryConnect(ConnectQueryDTO.builder().userId(order.getMemberId())
                            .unionType(SourceEnum.WECHAT_MP_OPEN_ID.name()).build());
                    if (connect == null || blank(connect.getUnionId())) throw failure("MISSING_OPENID", "支付会员的小程序身份关联缺失，请核查");
                    request.put("order_key", Map.of("order_number_type", 2, "transaction_id", order.getReceivableNo()));
                    request.put("payer", Map.of("openid", connect.getUnionId()));
                    String hash = DigestUtil.sha256Hex(JSON.toJSONString(request, JSONWriter.Feature.MapSortField));
                    log.setRequestHash(hash);
                    log.setSummary(summary(group, request));
                    if (hash.equals(sync.getSuccessHash())) {
                        outcome = "SKIPPED";
                    } else {
                        if (!hash.equals(sync.getRequestHash()) || blank(sync.getUploadTime())) {
                            sync.setUploadTime(DateTimeFormatter.ISO_OFFSET_DATE_TIME.format(ZonedDateTime.now(ZoneId.of("Asia/Shanghai"))));
                        }
                        sync.setAppId(payment.getMpAppId());
                        sync.setMchId(payment.getMchId());
                        sync.setRequestHash(hash);
                        journal.saveRequest(sync);
                        request.put("upload_time", sync.getUploadTime());
                        doPostWithJson("wxa/sec/order/upload_shipping_info", request);
                    }
                } else {
                    if (!"SUCCESS".equals(sync.getStatus())) throw pending("WAIT_UPLOAD", "发货上报尚未成功，暂不发送收货通知");
                    if (group.stream().anyMatch(o -> !"COMPLETED".equals(o.getOrderStatus()) || o.getCompleteTime() == null)) {
                        throw pending("WAIT_COMPLETE", "同笔支付尚有未完成订单或完成时间缺失，暂不通知收货");
                    }
                    if ("SUCCESS".equals(sync.getReceiveStatus())) {
                        outcome = "SKIPPED";
                    } else {
                        long receivedTime = group.stream().map(Order::getCompleteTime).mapToLong(Date::getTime).max().orElseThrow() / 1000;
                        log.setSummary("整笔支付的关联订单均已完成");
                        doPostWithJson("wxa/sec/order/notify_confirm_receive", Map.of("transaction_id", order.getReceivableNo(),
                                "merchant_id", payment.getMchId(), "received_time", receivedTime));
                    }
                }
            } catch (WechatOrderSyncException e) {
                outcome = e.isPending() ? "PENDING" : "FAILED";
                errorCode = e.getCode();
                message = safeMessage(e.getMessage());
            } catch (Exception e) {
                outcome = "FAILED";
                errorCode = "REQUEST_OR_STORAGE_ERROR";
                message = "请求或记录异常，结果可能尚未确认；请核查微信后台后重试，同版本请求保持原上传时间";
            }
            // 落库失败必须向调用方抛出，不能误报同步成功，也不能用失败日志覆盖真实外部结果。
            journal.finish(sync, log, outcome, errorCode, message);
            return outcome;
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new ServiceException("同步等待被中断，请稍后重试");
        } finally {
            if (locked && lock.isHeldByCurrentThread()) lock.unlock();
        }
    }

    /** 同一微信流水的全部子订单必须属于同一支付会员、渠道且已经支付。 */
    private List<Order> paymentGroup(Order order) {
        List<Order> group = orderService.list(new LambdaQueryWrapper<Order>().eq(Order::getReceivableNo, order.getReceivableNo()));
        if (group == null || group.isEmpty()) throw failure("EMPTY_GROUP", "未找到支付关联订单");
        if (group.stream().anyMatch(o -> !eligible(o) || !Objects.equals(order.getMemberId(), o.getMemberId())
                || !"PAID".equals(o.getPayStatus()))) {
            throw failure("INVALID_GROUP", "同笔支付的会员、渠道或支付状态不一致，请核查");
        }
        return group;
    }

    /** 仅接受服务端订单的真实渠道和支付方式，不信任页面提交值。 */
    private boolean eligible(Order order) {
        return "WECHAT_MP".equals(order.getClientType()) && "WECHAT".equals(order.getPaymentMethod());
    }

    /** 旧配置缺失或值为 null 时保留开启行为；只有显式 false 停止自动调用。 */
    private boolean isOrderAutoUploadEnabled() {
        Setting setting = settingService.get(SettingEnum.ORDER_SETTING.name());
        if (setting == null || blank(setting.getSettingValue())) return true;
        OrderSetting value = JSON.parseObject(setting.getSettingValue(), OrderSetting.class);
        return value == null || !Boolean.FALSE.equals(value.getWechatOrderAutoUploadEnabled());
    }

    /** 校验当前支付配置，只将小程序及商户标识快照写入同步表，不保存支付密钥。 */
    private WechatPaymentSetting paymentSetting() {
        Setting setting = settingService.get(SettingEnum.WECHAT_PAYMENT.name());
        WechatPaymentSetting value = setting == null || blank(setting.getSettingValue()) ? null
                : JSON.parseObject(setting.getSettingValue(), WechatPaymentSetting.class);
        if (value == null || blank(value.getMpAppId()) || blank(value.getMchId())) throw failure("MISSING_CONFIG", "小程序 AppID 或微信支付商户号未配置");
        return value;
    }

    /** 查询微信物流编码；同次执行内由调用方按名称缓存，不将未匹配物流强行提交。 */
    private String deliveryCode(String name, Map<String, String> codes) {
        if (codes.isEmpty()) {
            JSONObject response = doPostWithJson("cgi-bin/express/delivery/open_msg/get_delivery_list", Map.of());
            if (response.getJSONArray("delivery_list") != null) {
                for (Object raw : response.getJSONArray("delivery_list")) {
                    JSONObject delivery = JSON.parseObject(JSON.toJSONString(raw));
                    if (!blank(delivery.getString("delivery_name")) && !blank(delivery.getString("delivery_id"))) {
                        codes.put(delivery.getString("delivery_name"), delivery.getString("delivery_id"));
                    }
                }
            }
        }
        if (codes.containsKey(name)) return codes.get(name);
        throw failure("UNKNOWN_LOGISTICS", "物流公司未匹配到微信运力编码，请核查");
    }

    /**
     * 调用微信接口，15 秒超时；仅令牌失效允许刷新重试一次，URL 每次重新构造。
     * 不记录含令牌的 URL 或响应正文；仅明确 errcode=0 视作受理成功。
     */
    protected JSONObject doPostWithJson(String path, Map<String, ?> request) {
        for (int attempt = 0; attempt < 2; attempt++) {
            String token = wechatAccessTokenUtil.cgiAccessToken(ClientTypeEnum.WECHAT_MP);
            if (blank(token)) throw failure("MISSING_TOKEN", "未获取到小程序访问令牌，请检查配置");
            JSONObject result = postJson(path, request, token);
            if (result == null || !result.containsKey("errcode")) throw failure("INVALID_RESPONSE", "微信响应缺少结果码，请核查后重试");
            String code = result.getString("errcode");
            if ("0".equals(code)) return result;
            if (attempt == 0 && Set.of("40001", "40014", "42001").contains(code)) {
                wechatAccessTokenUtil.removeAccessToken(ClientTypeEnum.WECHAT_MP);
                continue;
            }
            throw failure(code, "微信接口拒绝：" + safeMessage(result.getString("errmsg")));
        }
        throw failure("TOKEN_EXPIRED", "微信访问令牌刷新后仍失效");
    }

    /** 单次 HTTP 调用，不输出带令牌 URL 或原始响应；令牌重试由外层统一限制次数。 */
    protected JSONObject postJson(String path, Map<String, ?> request, String token) {
        try (HttpResponse response = HttpRequest.post("https://api.weixin.qq.com/" + path + "?access_token=" + token)
                .timeout(15000).header("Content-Type", "application/json")
                .body(JSON.toJSONString(request)).execute()) {
            if (!response.isOk()) throw failure("HTTP_" + response.getStatus(), "微信接口 HTTP 请求未成功，请核查后重试");
            return JSON.parseObject(response.body());
        }
    }

    /** 日志摘要仅含商城订单号和配送数量，不保留收货地址、OpenID 或完整物流请求。 */
    private String summary(List<Order> group, Map<String, Object> request) {
        String orders = String.join(",", group.stream().map(Order::getSn).sorted().toList());
        if (orders.length() > 1500) orders = orders.substring(0, 1500);
        return "订单=" + orders + "；配送类型=" + request.get("logistics_type") + "；包裹数=" + ((List<?>) request.get("shipping_list")).size();
    }

    /** 脱敏外部错误文本，屏蔽 URL、长凭据和手机号，限制持久化长度。 */
    private String safeMessage(String message) {
        if (message == null) return "未知接口错误";
        String safe = message.replaceAll("https?://[^\\s]+", "[链接已隐藏]")
                .replaceAll("[A-Za-z0-9_-]{16,}", "[标识已隐藏]").replaceAll("[0-9]{7,}", "[号码已隐藏]");
        return safe.substring(0, Math.min(500, safe.length()));
    }

    /** 判断配置及流水字段是否为空。 */
    private boolean blank(String value) {
        return value == null || value.isBlank();
    }

    /** 构造需人工修正的失败结果。 */
    private WechatOrderSyncException failure(String code, String message) {
        return new WechatOrderSyncException(code, message, false);
    }

    /** 构造条件尚未满足的待上报结果。 */
    private WechatOrderSyncException pending(String code, String message) {
        return new WechatOrderSyncException(code, message, true);
    }
}
