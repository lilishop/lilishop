package cn.lili.modules.payment.kit.plugin.wechat;

import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
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
import cn.lili.modules.payment.kit.core.enums.SignType;
import cn.lili.modules.payment.kit.core.kit.HttpKit;
import cn.lili.modules.payment.kit.core.kit.IpKit;
import cn.lili.modules.payment.kit.core.kit.WxPayKit;
import cn.lili.modules.payment.kit.core.utils.DateTimeZoneUtil;
import cn.lili.modules.payment.kit.dto.PayParam;
import cn.lili.modules.payment.kit.dto.PaymentSuccessParams;
import cn.lili.modules.payment.kit.params.dto.CashierParam;
import cn.lili.modules.payment.service.PaymentService;
import cn.lili.modules.payment.service.RefundLogService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.WithdrawalSetting;
import cn.lili.modules.system.entity.dto.connect.WechatConnectSetting;
import cn.lili.modules.system.entity.dto.connect.dto.WechatConnectSettingItem;
import cn.lili.modules.system.entity.dto.payment.WechatPaymentSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.lili.modules.wallet.entity.dos.MemberWithdrawApply;
import cn.lili.modules.wallet.entity.dto.TransferResultDTO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.google.gson.Gson;
import com.wechat.pay.java.core.Config;
import com.wechat.pay.java.core.RSAAutoCertificateConfig;
import com.wechat.pay.java.core.RSAPublicKeyConfig;
import com.wechat.pay.java.core.exception.ValidationException;
import com.wechat.pay.java.core.notification.NotificationConfig;
import com.wechat.pay.java.core.notification.NotificationParser;
import com.wechat.pay.java.core.notification.RequestParam;
import com.wechat.pay.java.service.payments.app.AppService;
import com.wechat.pay.java.service.payments.h5.H5Service;
import com.wechat.pay.java.service.payments.h5.model.H5Info;
import com.wechat.pay.java.service.payments.h5.model.SceneInfo;
import com.wechat.pay.java.service.payments.jsapi.JsapiService;
import com.wechat.pay.java.service.payments.model.Transaction;
import com.wechat.pay.java.service.payments.nativepay.NativePayService;
import com.wechat.pay.java.service.payments.nativepay.model.Amount;
import com.wechat.pay.java.service.payments.nativepay.model.PrepayRequest;
import com.wechat.pay.java.service.payments.nativepay.model.PrepayResponse;
import com.wechat.pay.java.service.refund.RefundService;
import com.wechat.pay.java.service.refund.model.AmountReq;
import com.wechat.pay.java.service.refund.model.CreateRequest;
import com.wechat.pay.java.service.refund.model.Refund;
import com.wechat.pay.java.service.transferbatch.TransferBatchService;
import com.wechat.pay.java.service.transferbatch.model.InitiateBatchTransferRequest;
import com.wechat.pay.java.service.transferbatch.model.InitiateBatchTransferResponse;
import com.wechat.pay.java.service.transferbatch.model.TransferDetailInput;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
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

    /**
     * 收银台
     */
    @Autowired
    private CashierSupport cashierSupport;
    /**
     * 支付日志
     */
    @Autowired
    private PaymentService paymentService;
    /**
     * 缓存
     */
    @Autowired
    private Cache<String> cache;
    /**
     * 退款日志
     */
    @Autowired
    private RefundLogService refundLogService;
    /**
     * 配置
     */
    @Autowired
    private SettingService settingService;
    /**
     * 联合登陆
     */
    @Autowired
    private ConnectService connectService;
    /**
     * 联合登陆
     */
    @Autowired
    private OrderService orderService;


    @Override
    public ResultMessage<Object> h5pay(HttpServletRequest request, HttpServletResponse response1, PayParam payParam) {

        try {
            CashierParam cashierParam = cashierSupport.cashierParam(payParam);

            //支付参数准备
            SceneInfo sceneInfo = new SceneInfo();
            sceneInfo.setPayerClientIp(IpKit.getRealIp(request));
            H5Info h5Info = new H5Info();
            h5Info.setType("WAP");
            sceneInfo.setH5Info(h5Info);

            //支付金额
            Integer fen = CurrencyUtil.fen(cashierParam.getPrice());
            //第三方付款订单
            String outOrderNo = SnowFlake.getIdStr();
            //过期时间
            String timeExpire = DateTimeZoneUtil.dateToTimeZone(System.currentTimeMillis() + 1000 * 60 * 3);

            //回传数据
            String attach = java.net.URLEncoder.encode(JSONUtil.toJsonStr(payParam), StandardCharsets.UTF_8.name());


            WechatPaymentSetting setting = wechatPaymentSetting();
            String appid = setting.getH5AppId();
            if (appid == null) {
                throw new ServiceException(ResultCode.WECHAT_PAYMENT_NOT_SETTING);
            }

            Config config = null;
            if ("CERT".equals(setting.getPublicType())) {
                config = this.getCertificateConfig(setting);
            } else {
                config = this.getPublicKeyConfig(setting);
            }
            // 构建service
            H5Service service = new H5Service.Builder().config(config).build();

            com.wechat.pay.java.service.payments.h5.model.PrepayRequest prepayRequest = new com.wechat.pay.java.service.payments.h5.model.PrepayRequest();
            com.wechat.pay.java.service.payments.h5.model.Amount amount = new com.wechat.pay.java.service.payments.h5.model.Amount();
            amount.setTotal(fen);
            prepayRequest.setAmount(amount);
            prepayRequest.setAppid(appid);
            prepayRequest.setMchid(setting.getMchId());
            prepayRequest.setDescription(cashierParam.getDetail());
            prepayRequest.setNotifyUrl(notifyUrl(wechatPaymentSetting().getCallbackUrl(), PaymentMethodEnum.WECHAT));
            prepayRequest.setAttach(attach);
            prepayRequest.setTimeExpire(timeExpire);
            prepayRequest.setOutTradeNo(outOrderNo);
            prepayRequest.setSceneInfo(sceneInfo);
            // 调用下单方法，得到应答
            com.wechat.pay.java.service.payments.h5.model.PrepayResponse response = service.prepay(prepayRequest);
            updateOrderPayNo(payParam, outOrderNo);

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
                    ConnectQueryDTO.builder().userId(UserContext.getCurrentUser().getId()).unionType(SourceEnum.WECHAT_OFFIACCOUNT_OPEN_ID.name()).build()
            );
            if (connect == null) {
                return null;
            }


            CashierParam cashierParam = cashierSupport.cashierParam(payParam);

            //支付金额
            Integer fen = CurrencyUtil.fen(cashierParam.getPrice());
            //第三方付款订单
            String outOrderNo = SnowFlake.getIdStr();
            //过期时间
            String timeExpire = DateTimeZoneUtil.dateToTimeZone(System.currentTimeMillis() + 1000 * 60 * 3);

            // 将 Hutool URLEncoder 替换为标准库
            String attach = java.net.URLEncoder.encode(JSONUtil.toJsonStr(payParam), StandardCharsets.UTF_8.name());

            WechatPaymentSetting setting = wechatPaymentSetting();
            String appid = setting.getJsapiAppId();
            if (appid == null) {
                throw new ServiceException(ResultCode.WECHAT_PAYMENT_NOT_SETTING);
            }

            Config config = null;
            if ("CERT".equals(setting.getPublicType())) {
                config = this.getCertificateConfig(setting);
            } else {
                config = this.getPublicKeyConfig(setting);
            }
            // 构建service
            JsapiService service = new JsapiService.Builder().config(config).build();

            com.wechat.pay.java.service.payments.jsapi.model.PrepayRequest prepayRequest = new com.wechat.pay.java.service.payments.jsapi.model.PrepayRequest();
            com.wechat.pay.java.service.payments.jsapi.model.Amount amount = new com.wechat.pay.java.service.payments.jsapi.model.Amount();

            com.wechat.pay.java.service.payments.jsapi.model.Payer payer = new com.wechat.pay.java.service.payments.jsapi.model.Payer();
            payer.setOpenid(connect.getUnionId());
            amount.setTotal(fen);
            prepayRequest.setAmount(amount);
            prepayRequest.setAppid(appid);
            prepayRequest.setMchid(setting.getMchId());
            prepayRequest.setDescription(cashierParam.getDetail());
            prepayRequest.setNotifyUrl(notifyUrl(wechatPaymentSetting().getCallbackUrl(), PaymentMethodEnum.WECHAT));
            prepayRequest.setAttach(attach);
            prepayRequest.setTimeExpire(timeExpire);
            prepayRequest.setOutTradeNo(outOrderNo);
            prepayRequest.setPayer(payer);
            // 调用下单方法，得到应答
            com.wechat.pay.java.service.payments.jsapi.model.PrepayResponse response = service.prepay(prepayRequest);
            updateOrderPayNo(payParam, outOrderNo);

            Map<String, String> map = WxPayKit.jsApiCreateSign(appid, response.getPrepayId(), setting.getApiclientKey());
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

            CashierParam cashierParam = cashierSupport.cashierParam(payParam);

            //支付金额
            Integer fen = CurrencyUtil.fen(cashierParam.getPrice());
            //第三方付款订单
            String outOrderNo = SnowFlake.getIdStr();
            //过期时间
            String timeExpire = DateTimeZoneUtil.dateToTimeZone(System.currentTimeMillis() + 1000 * 60 * 3);

            // 将 Hutool URLEncoder 替换为标准库
            String attach = java.net.URLEncoder.encode(JSONUtil.toJsonStr(payParam), StandardCharsets.UTF_8.name());

            WechatPaymentSetting setting = wechatPaymentSetting();
            String appid = setting.getJsapiAppId();
            if (appid == null) {
                throw new ServiceException(ResultCode.WECHAT_PAYMENT_NOT_SETTING);
            }

            Config config = null;
            if ("CERT".equals(setting.getPublicType())) {
                config = this.getCertificateConfig(setting);
            } else {
                config = this.getPublicKeyConfig(setting);
            }
            // 构建service
            AppService service = new AppService.Builder().config(config).build();

            com.wechat.pay.java.service.payments.app.model.PrepayRequest prepayRequest = new com.wechat.pay.java.service.payments.app.model.PrepayRequest();
            com.wechat.pay.java.service.payments.app.model.Amount amount = new com.wechat.pay.java.service.payments.app.model.Amount();
            amount.setTotal(fen);
            prepayRequest.setAmount(amount);
            prepayRequest.setAppid(appid);
            prepayRequest.setMchid(setting.getMchId());
            prepayRequest.setDescription(cashierParam.getDetail());
            prepayRequest.setNotifyUrl(notifyUrl(wechatPaymentSetting().getCallbackUrl(), PaymentMethodEnum.WECHAT));
            prepayRequest.setAttach(attach);
            prepayRequest.setTimeExpire(timeExpire);
            prepayRequest.setOutTradeNo(outOrderNo);

            // 调用下单方法，得到应答
            com.wechat.pay.java.service.payments.app.model.PrepayResponse response = service.prepay(prepayRequest);
            updateOrderPayNo(payParam, outOrderNo);
            Map<String, String> map = WxPayKit.appPrepayIdCreateSign(appid,
                    setting.getMchId(),
                    response.getPrepayId(),
                    setting.getApiclientKey(), SignType.MD5);
            log.info("唤起支付参数:{}", map);
            //修改付款单号
            updateOrderPayNo(payParam, outOrderNo);
            return ResultUtil.data(map);
        } catch (Exception e) {
            log.error("支付异常", e);
            throw new ServiceException(ResultCode.PAY_ERROR);
        }
    }

    @Override
    public ResultMessage<Object> nativePay(HttpServletRequest request, PayParam payParam) {

        try {

            CashierParam cashierParam = cashierSupport.cashierParam(payParam);

            //支付金额
            Integer fen = CurrencyUtil.fen(cashierParam.getPrice());
            //第三方付款订单
            String outOrderNo = SnowFlake.getIdStr();
            //过期时间
            String timeExpire = DateTimeZoneUtil.dateToTimeZone(System.currentTimeMillis() + 1000 * 60 * 3);

            // 将 Hutool URLEncoder 替换为标准库
            String attach = java.net.URLEncoder.encode(JSONUtil.toJsonStr(payParam), StandardCharsets.UTF_8.name());

            WechatPaymentSetting setting = wechatPaymentSetting();

            String appid = setting.getNativeAppId();
            if (appid == null) {
                throw new ServiceException(ResultCode.WECHAT_PAYMENT_NOT_SETTING);
            }

            Config config = null;
            if ("CERT".equals(setting.getPublicType())) {
                config = this.getCertificateConfig(setting);
            } else {
                config = this.getPublicKeyConfig(setting);
            }
            // 构建service
            NativePayService service = new NativePayService.Builder().config(config).build();

            PrepayRequest prepayRequest = new PrepayRequest();
            Amount amount = new Amount();
            amount.setTotal(fen);
            prepayRequest.setAmount(amount);
            prepayRequest.setAppid(appid);
            prepayRequest.setMchid(setting.getMchId());
            prepayRequest.setDescription(cashierParam.getDetail());
            prepayRequest.setNotifyUrl(notifyUrl(wechatPaymentSetting().getCallbackUrl(), PaymentMethodEnum.WECHAT));
            prepayRequest.setAttach(attach);
            prepayRequest.setTimeExpire(timeExpire);
            prepayRequest.setOutTradeNo(outOrderNo);

            // 调用下单方法，得到应答
            PrepayResponse response = service.prepay(prepayRequest);
            updateOrderPayNo(payParam, outOrderNo);
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
                    ConnectQueryDTO.builder().userId(UserContext.getCurrentUser().getId()).unionType(SourceEnum.WECHAT_MP_OPEN_ID.name()).build()
            );
            if (connect == null) {
                return null;
            }

            com.wechat.pay.java.service.payments.jsapi.model.Payer payer = new com.wechat.pay.java.service.payments.jsapi.model.Payer();
            payer.setOpenid(connect.getUnionId());

            CashierParam cashierParam = cashierSupport.cashierParam(payParam);

            //支付金额
            Integer fen = CurrencyUtil.fen(cashierParam.getPrice());
            //第三方付款订单
            String outOrderNo = SnowFlake.getIdStr();
            //过期时间
            String timeExpire = DateTimeZoneUtil.dateToTimeZone(System.currentTimeMillis() + 1000 * 60 * 3);

            //微信小程序，appid 需要单独获取，这里读取了联合登陆配置的appid ，实际场景小程序自动登录，所以这个appid是最为保险的做法
            //如果有2开需求，这里需要调整，修改这个appid的获取途径即可
            String appid = wechatPaymentSetting().getMpAppId();
            if (StringUtils.isEmpty(appid)) {
                throw new ServiceException(ResultCode.WECHAT_PAYMENT_NOT_SETTING);
            }
            // 将 Hutool URLEncoder 替换为标准库
            String attach = java.net.URLEncoder.encode(JSONUtil.toJsonStr(payParam), StandardCharsets.UTF_8.name());

            WechatPaymentSetting setting = wechatPaymentSetting();

            Config config = null;
            if ("CERT".equals(setting.getPublicType())) {
                config = this.getCertificateConfig(setting);
            } else {
                config = this.getPublicKeyConfig(setting);
            }
            // 构建service
            JsapiService service = new JsapiService.Builder().config(config).build();

            com.wechat.pay.java.service.payments.jsapi.model.PrepayRequest prepayRequest = new com.wechat.pay.java.service.payments.jsapi.model.PrepayRequest();
            com.wechat.pay.java.service.payments.jsapi.model.Amount amount = new com.wechat.pay.java.service.payments.jsapi.model.Amount();
            amount.setTotal(fen);
            prepayRequest.setAmount(amount);
            prepayRequest.setAppid(appid);
            prepayRequest.setMchid(setting.getMchId());
            prepayRequest.setDescription(cashierParam.getDetail());
            prepayRequest.setNotifyUrl(notifyUrl(wechatPaymentSetting().getCallbackUrl(), PaymentMethodEnum.WECHAT));
            prepayRequest.setAttach(attach);
            prepayRequest.setTimeExpire(timeExpire);
            prepayRequest.setOutTradeNo(outOrderNo);
            prepayRequest.setPayer(payer);
            // 调用下单方法，得到应答
            com.wechat.pay.java.service.payments.jsapi.model.PrepayResponse response = service.prepay(prepayRequest);
            updateOrderPayNo(payParam, outOrderNo);

            Map<String, String> map = WxPayKit.jsApiCreateSign(appid, response.getPrepayId(), setting.getApiclientKey());
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
        try {
            verifyNotify(request);
        } catch (Exception e) {
            log.error("支付异常", e);
        }
    }

    /**
     * 微信提现
     * 文档地址：https://pay.weixin.qq.com/docs/merchant/apis/batch-transfer-to-balance/transfer-batch/initiate-batch-transfer.html
     *
     * @param memberWithdrawApply 会员提现申请
     */
    @Override
    public TransferResultDTO transfer(MemberWithdrawApply memberWithdrawApply) {
        try {
            //获取提现设置
            WithdrawalSetting withdrawalSetting = new Gson().fromJson(settingService.get(SettingEnum.WITHDRAWAL_SETTING.name()).getSettingValue(),
                    WithdrawalSetting.class);

            //获取用户OPENID
            WechatConnectSetting wechatConnectSetting = new Gson().fromJson(settingService.get(SettingEnum.WECHAT_CONNECT.name()).getSettingValue()
                    , WechatConnectSetting.class);
            String source = "";
            for (WechatConnectSettingItem wechatConnectSettingItem : wechatConnectSetting.getWechatConnectSettingItems()) {
                if (wechatConnectSettingItem.getAppId().equals(withdrawalSetting.getWechatAppId())) {
                    switch (wechatConnectSettingItem.getClientType()) {
                        case "PC":
                            source = SourceEnum.WECHAT_PC_OPEN_ID.name();
                            break;
                        case "H5":
                            source = SourceEnum.WECHAT_OFFIACCOUNT_OPEN_ID.name();
                            break;
                        case "MP":
                            source = SourceEnum.WECHAT_MP_OPEN_ID.name();
                            break;
                        case "APP":
                            source = SourceEnum.WECHAT_APP_OPEN_ID.name();
                            break;
                    }
                }
            }


            //获取用户openId
            Connect connect = connectService.queryConnect(
                    ConnectQueryDTO.builder().userId(memberWithdrawApply.getMemberId())
                            .unionType(source).build()
            );
            //获取微信设置
            WechatPaymentSetting setting = wechatPaymentSetting();

            Config config = null;
            if ("CERT".equals(setting.getPublicType())) {
                config = this.getCertificateConfig(setting);
            } else {
                config = this.getPublicKeyConfig(setting);
            }
            // 构建service
            TransferBatchService service = new TransferBatchService.Builder().config(config).build();

            InitiateBatchTransferRequest request = new InitiateBatchTransferRequest();
            request.setAppid(withdrawalSetting.getWechatAppId());
            request.setOutBatchNo(SnowFlake.createStr("T"));
            request.setBatchName("用户提现");
            request.setBatchRemark("用户提现");
            request.setTotalAmount(CurrencyUtil.getFenLong(memberWithdrawApply.getApplyMoney()));
            request.setTotalNum(1);
            request.setTransferSceneId("1000");

            List<TransferDetailInput> transferDetailListList = new ArrayList<>();
            {
                TransferDetailInput transferDetailInput = new TransferDetailInput();
                transferDetailInput.setOutDetailNo(SnowFlake.createStr("TD"));
                transferDetailInput.setTransferAmount(CurrencyUtil.getFenLong(memberWithdrawApply.getApplyMoney()));
                transferDetailInput.setTransferRemark("用户提现");
                transferDetailInput.setOpenid(connect.getUnionId());
                transferDetailListList.add(transferDetailInput);
            }
            request.setTransferDetailList(transferDetailListList);

            // 调用下单方法，得到应答
            InitiateBatchTransferResponse response = service.initiateBatchTransfer(request);
            log.info("微信提现响应 {}", response);


            return TransferResultDTO.builder().result(response.getBatchId() != null).build();
            //根据自身业务进行接下来的任务处理
        } catch (Exception e) {
            e.printStackTrace();
            return TransferResultDTO.builder().result(false).response(e.getMessage()).build();
        }

    }

    /**
     * 验证结果，执行支付回调
     *
     * @param request
     * @throws Exception
     */
    private void verifyNotify(HttpServletRequest request) throws Exception {

        // 构造 RequestParam
        RequestParam requestParam = new RequestParam.Builder()
                .serialNumber(request.getHeader("Wechatpay-Serial"))
                .nonce(request.getHeader("Wechatpay-Nonce"))
                .signature(request.getHeader("Wechatpay-Signature"))
                .timestamp(request.getHeader("Wechatpay-Timestamp"))
                .body(HttpKit.readData(request))
                .build();

        WechatPaymentSetting setting = wechatPaymentSetting();
        NotificationConfig config = null;
        if ("CERT".equals(setting.getPublicType())) {
            config = new RSAAutoCertificateConfig.Builder()
                    .merchantId(setting.getMchId())
                    .privateKey(setting.getApiclientKey())
                    .merchantSerialNumber(setting.getSerialNumber())
                    .apiV3Key(setting.getApiKey3())
                    .build();
        } else {
            config = new RSAPublicKeyConfig.Builder()
                    .merchantId(setting.getMchId())
                    .apiV3Key(setting.getApiKey3())
                    .privateKey(setting.getApiclientKey())
                    .merchantSerialNumber(setting.getSerialNumber())
                    .publicKeyId(setting.getPublicId())
                    .publicKey(setting.getPublicKey())
                    .build();
        }

        // 初始化 NotificationParser
        NotificationParser parser = new NotificationParser(config);

        try {
            // 以支付通知回调为例，验签、解密并转换成 Transaction
            Transaction transaction = parser.parse(requestParam, Transaction.class);

            // 将 Hutool URLDecoder 替换为标准库
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
        } catch (ValidationException e) {
            // 签名验证失败，返回 401 UNAUTHORIZED 状态码
            log.error("sign verification failed", e);

        }

    }

    @Override
    public void refund(RefundLog refundLog) {

        try {

            AmountReq amount = new AmountReq();
            amount.setRefund(CurrencyUtil.getFenLong(refundLog.getTotalAmount()));
            amount.setTotal(CurrencyUtil.getFenLong(orderService.getPaymentTotal(refundLog.getOrderSn())));
            amount.setCurrency("CNY");
            //获取微信设置
            WechatPaymentSetting setting = wechatPaymentSetting();

            Config config = null;
            if ("CERT".equals(setting.getPublicType())) {
                config = this.getCertificateConfig(setting);
            } else {
                config = this.getPublicKeyConfig(setting);
            }
            // 构建service
            RefundService refundService = new RefundService.Builder().config(config).build();

            CreateRequest request = new CreateRequest();
            request.setTransactionId(refundLog.getPaymentReceivableNo());
            request.setAmount(amount);
            request.setOutRefundNo(refundLog.getOutOrderNo());
            request.setReason(refundLog.getRefundReason());
            request.setNotifyUrl(refundNotifyUrl(wechatPaymentSetting().getCallbackUrl(), PaymentMethodEnum.WECHAT));

            Refund refund = refundService.create(request);

            log.info("微信退款响应 {}", refund);
            refundLogService.save(refundLog);
        } catch (Exception e) {
            log.error("微信退款申请失败", e);
        }

    }

    @Override
    public void refundNotify(HttpServletRequest request) {
        // 构造 RequestParam
        RequestParam requestParam = new RequestParam.Builder()
                .serialNumber(request.getHeader("Wechatpay-Serial"))
                .nonce(request.getHeader("Wechatpay-Nonce"))
                .signature(request.getHeader("Wechatpay-Signature"))
                .timestamp(request.getHeader("Wechatpay-Timestamp"))
                .body(HttpKit.readData(request))
                .build();

        WechatPaymentSetting setting = wechatPaymentSetting();
        NotificationConfig config = null;
        if ("CERT".equals(setting.getPublicType())) {
            config = new RSAAutoCertificateConfig.Builder()
                    .merchantId(setting.getMchId())
                    .privateKey(setting.getApiclientKey())
                    .merchantSerialNumber(setting.getSerialNumber())
                    .apiV3Key(setting.getApiKey3())
                    .build();
        } else {
            config = new RSAPublicKeyConfig.Builder()
                    .merchantId(setting.getMchId())
                    .apiV3Key(setting.getApiKey3())
                    .privateKey(setting.getApiclientKey())
                    .merchantSerialNumber(setting.getSerialNumber())
                    .publicKeyId(setting.getPublicId())
                    .publicKey(setting.getPublicKey())
                    .build();
        }

        // 初始化 NotificationParser
        NotificationParser parser = new NotificationParser(config);
        try {
            Refund refund = parser.parse(requestParam, Refund.class);
            RefundLog refundLog = refundLogService.getOne(new LambdaQueryWrapper<RefundLog>().eq(RefundLog::getPaymentReceivableNo,
                    refund.getTransactionId()));
            if (refundLog != null) {
                refundLog.setIsRefund(true);
                refundLog.setReceivableNo(refund.getRefundId());
                refundLogService.saveOrUpdate(refundLog);
            }
        } catch (Exception e) {
            log.error("微信退款失败", e);
        }
    }

    /**
     * 获取微信支付配置
     *
     * @return
     */
    private WechatPaymentSetting wechatPaymentSetting() {
        try {
            Setting systemSetting = settingService.get(SettingEnum.WECHAT_PAYMENT.name());
            WechatPaymentSetting wechatPaymentSetting = JSONUtil.toBean(systemSetting.getSettingValue(), WechatPaymentSetting.class);
            return wechatPaymentSetting;
        } catch (Exception e) {
            log.error("微信支付暂不支持", e);
            throw new ServiceException(ResultCode.PAY_NOT_SUPPORT);
        }
    }

    /**
     * 获取微信公钥配置
     *
     * @param setting
     * @return
     */
    private RSAPublicKeyConfig getPublicKeyConfig(WechatPaymentSetting setting) {
        return
                new RSAPublicKeyConfig.Builder()
                        .merchantId(setting.getMchId())
                        .privateKey(setting.getApiclientKey())
                        .publicKey(setting.getPublicKey())
                        .publicKeyId(setting.getPublicId())
                        .merchantSerialNumber(setting.getSerialNumber())
                        .apiV3Key(setting.getApiKey3())
                        .build();
    }

    /**
     * 获取微信证书配置
     *
     * @param setting
     * @return
     */
    private RSAAutoCertificateConfig getCertificateConfig(WechatPaymentSetting setting) {
        return new RSAAutoCertificateConfig.Builder()
                .merchantId(setting.getMchId())
                .privateKey(setting.getApiclientKey())
                .merchantSerialNumber(setting.getSerialNumber())
                .apiV3Key(setting.getApiKey3())
                .build();
    }

    /**
     * 修改订单支付单号
     *
     * @param payParam   支付参数
     * @param outOrderNo 订单号
     */
    private void updateOrderPayNo(PayParam payParam, String outOrderNo) {
        if ("ORDER".equals(payParam.getOrderType())) {
            orderService.update(new LambdaUpdateWrapper<Order>()
                    .eq(Order::getSn, payParam.getSn())
                    .set(Order::getPayOrderNo, outOrderNo));
        } else if ("TRADE".equals(payParam.getOrderType())) {
            orderService.update(new LambdaUpdateWrapper<Order>()
                    .eq(Order::getTradeSn, payParam.getSn())
                    .set(Order::getPayOrderNo, outOrderNo));
        }
    }
}
