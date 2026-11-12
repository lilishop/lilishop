package cn.lili.modules.payment.kit.plugin.wechat;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.context.ThreadContextHolder;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.common.utils.SnowFlake;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.connect.entity.Connect;
import cn.lili.modules.connect.entity.enums.SourceEnum;
import cn.lili.modules.connect.service.ConnectService;
import cn.lili.modules.member.entity.dto.ConnectQueryDTO;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.payment.entity.RefundLog;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.modules.payment.kit.CashierSupport;
import cn.lili.modules.payment.kit.Payment;
import cn.lili.modules.payment.kit.core.kit.HttpKit;
import cn.lili.modules.payment.kit.core.kit.IpKit;
import cn.lili.modules.payment.kit.core.utils.DateTimeZoneUtil;
import cn.lili.modules.payment.kit.dto.PayParam;
import cn.lili.modules.payment.kit.dto.PaymentQueryResult;
import cn.lili.modules.payment.kit.dto.PaymentSuccessParams;
import cn.lili.modules.payment.kit.params.dto.CashierParam;
import cn.lili.modules.payment.service.PaymentService;
import cn.lili.modules.system.entity.dto.payment.WechatPaymentSetting;
import cn.lili.modules.wallet.entity.dos.MemberWithdrawApply;
import cn.lili.modules.wallet.entity.dto.TransferResultDTO;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.google.gson.Gson;
import com.wechat.pay.java.core.Config;
import com.wechat.pay.java.core.exception.ValidationException;
import com.wechat.pay.java.core.notification.NotificationConfig;
import com.wechat.pay.java.core.notification.NotificationParser;
import com.wechat.pay.java.core.notification.RequestParam;
import com.wechat.pay.java.service.payments.app.AppServiceExtension;
import com.wechat.pay.java.service.payments.h5.H5Service;
import com.wechat.pay.java.service.payments.h5.model.H5Info;
import com.wechat.pay.java.service.payments.h5.model.SceneInfo;
import com.wechat.pay.java.service.payments.jsapi.JsapiServiceExtension;
import com.wechat.pay.java.service.payments.model.Transaction;
import com.wechat.pay.java.service.payments.nativepay.NativePayService;
import com.wechat.pay.java.service.payments.nativepay.model.Amount;
import com.wechat.pay.java.service.payments.nativepay.model.PrepayRequest;
import com.wechat.pay.java.service.payments.nativepay.model.PrepayResponse;
import lombok.Builder;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 微信支付
 *
 * @author Chopper
 * @since 2020/12/21 17:44
 */
@Slf4j
@Component
public class WechatPlugin implements Payment {

    @Autowired
    private CashierSupport cashierSupport;

    @Autowired
    private PaymentService paymentService;

    @Autowired
    private ConnectService connectService;

    @Autowired
    private OrderService orderService;

    @Autowired
    private WechatConfigSupport wechatConfigSupport;

    @Autowired
    private WechatTransferSupport wechatTransferSupport;

    @Autowired
    private WechatRefundSupport wechatRefundSupport;

    @Autowired
    private WechatOrderQuerySupport wechatOrderQuerySupport;

    @Override
    public ResultMessage<Object> h5pay(HttpServletRequest request, HttpServletResponse response1, PayParam payParam) {
        try {
            WechatPrepayContext ctx = buildPrepayContext(payParam);
            String appid = ctx.getSetting().getH5AppId();
            if (appid == null) {
                throw new ServiceException(ResultCode.WECHAT_PAYMENT_NOT_SETTING);
            }

            SceneInfo sceneInfo = new SceneInfo();
            sceneInfo.setPayerClientIp(IpKit.getRealIp(request));
            H5Info h5Info = new H5Info();
            h5Info.setType("WAP");
            sceneInfo.setH5Info(h5Info);

            H5Service service = new H5Service.Builder().config(ctx.getConfig()).build();
            com.wechat.pay.java.service.payments.h5.model.PrepayRequest prepayRequest = new com.wechat.pay.java.service.payments.h5.model.PrepayRequest();
            com.wechat.pay.java.service.payments.h5.model.Amount amount = new com.wechat.pay.java.service.payments.h5.model.Amount();
            amount.setTotal(ctx.getFen());
            prepayRequest.setAmount(amount);
            prepayRequest.setAppid(appid);
            prepayRequest.setMchid(ctx.getSetting().getMchId());
            prepayRequest.setDescription(ctx.getCashierParam().getDetail());
            prepayRequest.setNotifyUrl(notifyUrl(ctx.getSetting().getCallbackUrl(), PaymentMethodEnum.WECHAT));
            prepayRequest.setAttach(ctx.getAttach());
            prepayRequest.setTimeExpire(ctx.getTimeExpire());
            prepayRequest.setOutTradeNo(ctx.getOutOrderNo());
            prepayRequest.setSceneInfo(sceneInfo);

            com.wechat.pay.java.service.payments.h5.model.PrepayResponse response = service.prepay(prepayRequest);
            updateOrderPayNo(payParam, ctx.getOutOrderNo());
            return ResultUtil.data(response.getH5Url());
        } catch (Exception e) {
            log.error("微信H5支付错误", e);
            throw new ServiceException(ResultCode.PAY_ERROR);
        }
    }

    @Override
    public ResultMessage<Object> jsApiPay(HttpServletRequest request, PayParam payParam) {
        try {
            Connect connect = connectService.queryConnect(
                    ConnectQueryDTO.builder()
                            .userId(UserContext.getCurrentUser().getId())
                            .unionType(SourceEnum.WECHAT_OFFIACCOUNT_OPEN_ID.name())
                            .build()
            );
            if (connect == null) {
                return null;
            }

            WechatPrepayContext ctx = buildPrepayContext(payParam);
            String appid = ctx.getSetting().getJsapiAppId();
            if (appid == null) {
                throw new ServiceException(ResultCode.WECHAT_PAYMENT_NOT_SETTING);
            }

            JsapiServiceExtension service = new JsapiServiceExtension.Builder().config(ctx.getConfig()).build();
            com.wechat.pay.java.service.payments.jsapi.model.PrepayRequest prepayRequest = new com.wechat.pay.java.service.payments.jsapi.model.PrepayRequest();
            com.wechat.pay.java.service.payments.jsapi.model.Amount amount = new com.wechat.pay.java.service.payments.jsapi.model.Amount();
            com.wechat.pay.java.service.payments.jsapi.model.Payer payer = new com.wechat.pay.java.service.payments.jsapi.model.Payer();
            payer.setOpenid(connect.getUnionId());
            amount.setTotal(ctx.getFen());
            prepayRequest.setAmount(amount);
            prepayRequest.setAppid(appid);
            prepayRequest.setMchid(ctx.getSetting().getMchId());
            prepayRequest.setDescription(ctx.getCashierParam().getDetail());
            prepayRequest.setNotifyUrl(notifyUrl(ctx.getSetting().getCallbackUrl(), PaymentMethodEnum.WECHAT));
            prepayRequest.setAttach(ctx.getAttach());
            prepayRequest.setTimeExpire(ctx.getTimeExpire());
            prepayRequest.setOutTradeNo(ctx.getOutOrderNo());
            prepayRequest.setPayer(payer);

            com.wechat.pay.java.service.payments.jsapi.model.PrepayWithRequestPaymentResponse response =
                    service.prepayWithRequestPayment(prepayRequest);
            updateOrderPayNo(payParam, ctx.getOutOrderNo());

            Map<String, String> map = WechatRequestPaymentParamConverter.fromJsapiResponse(response);
            log.info("唤起支付参数:{}", map);
            return ResultUtil.data(map);
        } catch (Exception e) {
            log.error("支付异常", e);
            throw new ServiceException(ResultCode.PAY_ERROR);
        }
    }

    @Override
    public ResultMessage<Object> appPay(HttpServletRequest request, PayParam payParam) {
        try {
            WechatPrepayContext ctx = buildPrepayContext(payParam);
            String appid = ctx.getSetting().getAppAppId();
            if (appid == null) {
                throw new ServiceException(ResultCode.WECHAT_PAYMENT_NOT_SETTING);
            }

            AppServiceExtension service = new AppServiceExtension.Builder().config(ctx.getConfig()).build();
            com.wechat.pay.java.service.payments.app.model.PrepayRequest prepayRequest = new com.wechat.pay.java.service.payments.app.model.PrepayRequest();
            com.wechat.pay.java.service.payments.app.model.Amount amount = new com.wechat.pay.java.service.payments.app.model.Amount();
            amount.setTotal(ctx.getFen());
            prepayRequest.setAmount(amount);
            prepayRequest.setAppid(appid);
            prepayRequest.setMchid(ctx.getSetting().getMchId());
            prepayRequest.setDescription(ctx.getCashierParam().getDetail());
            prepayRequest.setNotifyUrl(notifyUrl(ctx.getSetting().getCallbackUrl(), PaymentMethodEnum.WECHAT));
            prepayRequest.setAttach(ctx.getAttach());
            prepayRequest.setTimeExpire(ctx.getTimeExpire());
            prepayRequest.setOutTradeNo(ctx.getOutOrderNo());

            com.wechat.pay.java.service.payments.app.model.PrepayWithRequestPaymentResponse response =
                    service.prepayWithRequestPayment(prepayRequest);
            updateOrderPayNo(payParam, ctx.getOutOrderNo());
            Map<String, String> map = WechatRequestPaymentParamConverter.fromAppResponse(response);
            log.info("唤起支付参数:{}", map);
            return ResultUtil.data(map);
        } catch (Exception e) {
            log.error("支付异常", e);
            throw new ServiceException(ResultCode.PAY_ERROR);
        }
    }

    @Override
    public ResultMessage<Object> nativePay(HttpServletRequest request, PayParam payParam) {
        try {
            WechatPrepayContext ctx = buildPrepayContext(payParam);
            String appid = ctx.getSetting().getNativeAppId();
            if (appid == null) {
                throw new ServiceException(ResultCode.WECHAT_PAYMENT_NOT_SETTING);
            }

            NativePayService service = new NativePayService.Builder().config(ctx.getConfig()).build();
            PrepayRequest prepayRequest = new PrepayRequest();
            Amount amount = new Amount();
            amount.setTotal(ctx.getFen());
            prepayRequest.setAmount(amount);
            prepayRequest.setAppid(appid);
            prepayRequest.setMchid(ctx.getSetting().getMchId());
            prepayRequest.setDescription(ctx.getCashierParam().getDetail());
            prepayRequest.setNotifyUrl(notifyUrl(ctx.getSetting().getCallbackUrl(), PaymentMethodEnum.WECHAT));
            prepayRequest.setAttach(ctx.getAttach());
            prepayRequest.setTimeExpire(ctx.getTimeExpire());
            prepayRequest.setOutTradeNo(ctx.getOutOrderNo());

            PrepayResponse response = service.prepay(prepayRequest);
            updateOrderPayNo(payParam, ctx.getOutOrderNo());
            return ResultUtil.data(response.getCodeUrl());
        } catch (ServiceException e) {
            log.error("支付异常", e);
            throw new ServiceException(ResultCode.PAY_ERROR);
        } catch (Exception e) {
            log.error("支付异常", e);
            throw new ServiceException(ResultCode.PAY_ERROR);
        }
    }

    @Override
    public ResultMessage<Object> mpPay(HttpServletRequest request, PayParam payParam) {
        try {
            Connect connect = connectService.queryConnect(
                    ConnectQueryDTO.builder()
                            .userId(UserContext.getCurrentUser().getId())
                            .unionType(SourceEnum.WECHAT_MP_OPEN_ID.name())
                            .build()
            );
            if (connect == null) {
                return null;
            }

            WechatPrepayContext ctx = buildPrepayContext(payParam);
            String appid = ctx.getSetting().getMpAppId();
            if (StringUtils.isEmpty(appid)) {
                throw new ServiceException(ResultCode.WECHAT_PAYMENT_NOT_SETTING);
            }

            com.wechat.pay.java.service.payments.jsapi.model.Payer payer = new com.wechat.pay.java.service.payments.jsapi.model.Payer();
            payer.setOpenid(connect.getUnionId());

            JsapiServiceExtension service = new JsapiServiceExtension.Builder().config(ctx.getConfig()).build();
            com.wechat.pay.java.service.payments.jsapi.model.PrepayRequest prepayRequest = new com.wechat.pay.java.service.payments.jsapi.model.PrepayRequest();
            com.wechat.pay.java.service.payments.jsapi.model.Amount amount = new com.wechat.pay.java.service.payments.jsapi.model.Amount();
            amount.setTotal(ctx.getFen());
            prepayRequest.setAmount(amount);
            prepayRequest.setAppid(appid);
            prepayRequest.setMchid(ctx.getSetting().getMchId());
            prepayRequest.setDescription(ctx.getCashierParam().getDetail());
            prepayRequest.setNotifyUrl(notifyUrl(ctx.getSetting().getCallbackUrl(), PaymentMethodEnum.WECHAT));
            prepayRequest.setAttach(ctx.getAttach());
            prepayRequest.setTimeExpire(ctx.getTimeExpire());
            prepayRequest.setOutTradeNo(ctx.getOutOrderNo());
            prepayRequest.setPayer(payer);

            com.wechat.pay.java.service.payments.jsapi.model.PrepayWithRequestPaymentResponse response =
                    service.prepayWithRequestPayment(prepayRequest);
            updateOrderPayNo(payParam, ctx.getOutOrderNo());

            Map<String, String> map = WechatRequestPaymentParamConverter.fromJsapiResponse(response);
            log.info("唤起支付参数:{}", map);
            return ResultUtil.data(map);
        } catch (Exception e) {
            log.error("支付异常", e);
            throw new ServiceException(ResultCode.PAY_ERROR);
        }
    }

    @Override
    public void callBack(HttpServletRequest request) {
        try {
            verifyNotify(request);
        } catch (Exception e) {
            log.error("支付异常", e);
        }
    }

    @Override
    public void notify(HttpServletRequest request) {
        notify(request, ThreadContextHolder.getHttpResponse());
    }

    @Override
    public void notify(HttpServletRequest request, HttpServletResponse response) {
        try {
            verifyNotify(request);
            if (response != null) {
                response.setStatus(HttpServletResponse.SC_OK);
                response.setContentType("application/json;charset=UTF-8");
                Map<String, String> result = new HashMap<>(4);
                result.put("code", "SUCCESS");
                result.put("message", "成功");
                response.getWriter().write(new Gson().toJson(result));
                response.getWriter().flush();
            }
        } catch (ValidationException e) {
            log.error("微信支付通知验签失败", e);
            if (response != null) {
                response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
                response.setContentType("application/json;charset=UTF-8");
                Map<String, String> result = new HashMap<>(4);
                result.put("code", "FAIL");
                result.put("message", "签名验证失败");
                try {
                    response.getWriter().write(new Gson().toJson(result));
                    response.getWriter().flush();
                } catch (Exception writeEx) {
                    log.error("微信支付响应写入异常", writeEx);
                }
            }
        } catch (Exception e) {
            log.error("微信支付通知处理异常", e);
            if (response != null) {
                response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
                response.setContentType("application/json;charset=UTF-8");
                Map<String, String> result = new HashMap<>(4);
                result.put("code", "FAIL");
                result.put("message", e.getMessage() != null ? e.getMessage() : "系统异常");
                try {
                    response.getWriter().write(new Gson().toJson(result));
                    response.getWriter().flush();
                } catch (Exception writeEx) {
                    log.error("微信支付响应写入异常", writeEx);
                }
            }
        }
    }

    @Override
    public void transferNotify(HttpServletRequest request) {
        wechatTransferSupport.transferNotify(request);
    }

    @Override
    public TransferResultDTO transfer(MemberWithdrawApply memberWithdrawApply) {
        WechatPaymentSetting setting = wechatConfigSupport.getPaymentSetting();
        return wechatTransferSupport.transfer(memberWithdrawApply, transferNotifyUrl(setting.getCallbackUrl(), PaymentMethodEnum.WECHAT));
    }

    /**
     * 验证结果，执行支付回调
     *
     * @param request HTTP 请求
     * @throws Exception 验签失败或业务异常
     */
    void verifyNotify(HttpServletRequest request) throws Exception {
        RequestParam requestParam = new RequestParam.Builder()
                .serialNumber(request.getHeader("Wechatpay-Serial"))
                .nonce(request.getHeader("Wechatpay-Nonce"))
                .signature(request.getHeader("Wechatpay-Signature"))
                .timestamp(request.getHeader("Wechatpay-Timestamp"))
                .body(HttpKit.readData(request))
                .build();

        WechatPaymentSetting setting = wechatConfigSupport.getPaymentSetting();
        NotificationConfig config = wechatConfigSupport.getNotificationConfig(setting);
        NotificationParser parser = new NotificationParser(config);

        // 以支付通知回调为例，验签、解密并转换成 Transaction
        Transaction transaction = parser.parse(requestParam, Transaction.class);

        // 1. 校验商户号 mchid 是否匹配本商户配置
        if (CharSequenceUtil.isEmpty(transaction.getMchid()) || !transaction.getMchid().equals(setting.getMchId())) {
            log.error("微信支付异步通知：商户号不匹配，通知 mchid={}, 系统配置 mchid={}", transaction.getMchid(), setting.getMchId());
            throw new ServiceException(ResultCode.PAY_ERROR, "微信支付商户号不匹配");
        }

        // 2. 校验应用ID appid 是否属于系统配置的合法 AppID 范围
        List<String> validAppIds = java.util.stream.Stream.of(
                setting.getJsapiAppId(), setting.getNativeAppId(), setting.getMpAppId(),
                setting.getH5AppId(), setting.getAppAppId()
        ).filter(CharSequenceUtil::isNotEmpty).toList();
        if (!validAppIds.isEmpty() && !validAppIds.contains(transaction.getAppid())) {
            log.error("微信支付异步通知：应用ID不匹配，通知 appid={}, 系统配置 appIds={}", transaction.getAppid(), validAppIds);
            throw new ServiceException(ResultCode.PAY_ERROR, "微信支付应用ID不匹配");
        }

        // 3. 校验交易状态 trade_state 是否为成功状态
        if (transaction.getTradeState() == null || !Transaction.TradeStateEnum.SUCCESS.equals(transaction.getTradeState())) {
            log.warn("微信支付异步通知：非成功交易状态 trade_state={}", transaction.getTradeState());
            return;
        }

        String payParamJson = java.net.URLDecoder.decode(transaction.getAttach(), StandardCharsets.UTF_8.name());
        PayParam payParam = new Gson().fromJson(payParamJson, PayParam.class);
        Double totalAmount = CurrencyUtil.reversalFen(transaction.getAmount().getTotal());

        PaymentSuccessParams paymentSuccessParams = new PaymentSuccessParams(
                PaymentMethodEnum.WECHAT.name(),
                transaction.getTransactionId(),
                totalAmount,
                payParam
        );

        paymentService.success(paymentSuccessParams);
    }

    @Override
    public void refund(RefundLog refundLog) {
        WechatPaymentSetting setting = wechatConfigSupport.getPaymentSetting();
        wechatRefundSupport.refund(refundLog, refundNotifyUrl(setting.getCallbackUrl(), PaymentMethodEnum.WECHAT));
    }

    @Override
    public void refundNotify(HttpServletRequest request) {
        wechatRefundSupport.refundNotify(request);
    }

    @Override
    public PaymentQueryResult query(String outTradeNo) {
        return wechatOrderQuerySupport.query(outTradeNo);
    }

    @Override
    public void close(String outTradeNo) {
        wechatOrderQuerySupport.close(outTradeNo);
    }

    /**
     * 构建微信各终端预下单通用上下文
     *
     * @param payParam 支付入参
     * @return 预下单准备上下文
     * @throws Exception URL编码或配置解析异常
     */
    private WechatPrepayContext buildPrepayContext(PayParam payParam) throws Exception {
        CashierParam cashierParam = cashierSupport.cashierParam(payParam);
        Integer fen = CurrencyUtil.fen(cashierParam.getPrice());
        String outOrderNo = SnowFlake.getIdStr();
        String timeExpire = DateTimeZoneUtil.dateToTimeZone(System.currentTimeMillis() + 1000 * 60 * 3);
        String attach = java.net.URLEncoder.encode(JSONUtil.toJsonStr(payParam), StandardCharsets.UTF_8.name());
        WechatPaymentSetting setting = wechatConfigSupport.getPaymentSetting();
        Config config = wechatConfigSupport.getWechatConfig(setting);

        return WechatPrepayContext.builder()
                .cashierParam(cashierParam)
                .fen(fen)
                .outOrderNo(outOrderNo)
                .timeExpire(timeExpire)
                .attach(attach)
                .setting(setting)
                .config(config)
                .build();
    }

    /**
     * 更新订单渠道支付单号
     *
     * @param payParam   支付参数
     * @param outOrderNo 外部支付商户单号
     */
    private void updateOrderPayNo(PayParam payParam, String outOrderNo) {
        if ("ORDER".equals(payParam.getOrderType())) {
            orderService.update(new LambdaUpdateWrapper<Order>()
                    .eq(Order::getSn, payParam.getSn())
                    .set(Order::getPayOrderNo, outOrderNo)
                    .set(Order::getPaymentMethod, PaymentMethodEnum.WECHAT.name()));
        } else if ("TRADE".equals(payParam.getOrderType())) {
            orderService.update(new LambdaUpdateWrapper<Order>()
                    .eq(Order::getTradeSn, payParam.getSn())
                    .set(Order::getPayOrderNo, outOrderNo)
                    .set(Order::getPaymentMethod, PaymentMethodEnum.WECHAT.name()));
        }
    }

    /**
     * 微信预下单上下文参数封装
     *
     * @author Bulbasaur
     * @since 2026-11-12
     */
    @Getter
    @Builder
    private static class WechatPrepayContext {
        private final CashierParam cashierParam;
        private final Integer fen;
        private final String outOrderNo;
        private final String timeExpire;
        private final String attach;
        private final WechatPaymentSetting setting;
        private final Config config;
    }
}
