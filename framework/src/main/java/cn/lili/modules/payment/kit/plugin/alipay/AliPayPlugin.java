package cn.lili.modules.payment.kit.plugin.alipay;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.context.ThreadContextHolder;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.BeanUtil;
import cn.lili.common.utils.SnowFlake;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.payment.kit.dto.PaymentQueryResult;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import cn.lili.modules.payment.entity.RefundLog;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.modules.payment.kit.CashierSupport;
import cn.lili.modules.payment.kit.Payment;
import cn.lili.modules.payment.kit.dto.PayParam;
import cn.lili.modules.payment.kit.dto.PaymentSuccessParams;
import cn.lili.modules.payment.kit.params.dto.CashierParam;
import cn.lili.modules.payment.service.PaymentService;
import cn.lili.modules.payment.service.RefundLogService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.connect.ConnectSetting;
import cn.lili.modules.system.entity.dto.payment.AlipayPaymentSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.lili.modules.wallet.entity.dos.MemberWithdrawApply;
import cn.lili.modules.wallet.entity.dto.TransferResultDTO;
import cn.hutool.json.JSONObject;
import com.alipay.api.AlipayApiException;
import com.alipay.api.domain.*;
import com.alipay.api.internal.util.AlipaySignature;
import com.alipay.api.response.AlipayFundTransUniTransferResponse;
import com.alipay.api.response.AlipayTradeRefundResponse;
import com.alipay.api.response.AlipayTradeQueryResponse;
import com.alipay.api.response.AlipayTradeCloseResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.util.Map;

/**
 * 支付宝支付
 *
 * @author Chopper
 * @since 2020/12/17 09:55
 */
@Slf4j
@Component
public class AliPayPlugin implements Payment {
    /**
     * 支付日志
     */
    @Autowired
    private PaymentService paymentService;
    /**
     * 退款日志
     */
    @Autowired
    private RefundLogService refundLogService;
    /**
     * 收银台
     */
    @Autowired
    private CashierSupport cashierSupport;
    /**
     * 设置
     */
    @Autowired
    private SettingService settingService;
    /**
     * 订单服务
     */
    @Autowired
    private OrderService orderService;
    @Override
    public ResultMessage<Object> h5pay(HttpServletRequest request, HttpServletResponse response, PayParam payParam) {

        CashierParam cashierParam = cashierSupport.cashierParam(payParam);
        AlipayPaymentSetting alipayPaymentSetting = alipayPaymentSetting();
        //请求订单编号
        String outTradeNo = SnowFlake.getIdStr();
        //准备支付参数
        AlipayTradeWapPayModel payModel = new AlipayTradeWapPayModel();
        payModel.setBody(cashierParam.getTitle());
        payModel.setSubject(cashierParam.getDetail());
        payModel.setTotalAmount(cashierParam.getPrice() + "");
        //3分钟超时
        payModel.setTimeoutExpress("3m");
        payModel.setOutTradeNo(outTradeNo);
        payModel.setProductCode("QUICK_WAP_PAY");
        try {
            // Passback params moved into try to handle checked exception
            payModel.setPassbackParams(java.net.URLEncoder.encode(BeanUtil.formatKeyValuePair(payParam), "UTF-8"));

            log.info("支付宝H5支付：{}", JSONUtil.toJsonStr(payModel));
            updateOrderPayNo(payParam, outTradeNo);
            String returnUrl = callbackUrl(alipayPaymentSetting.getCallbackUrl(), PaymentMethodEnum.ALIPAY);
            if ("PAID_MEMBER".equals(String.valueOf(payParam.getOrderType()))) {
                returnUrl += "?paidMemberSn=" + java.net.URLEncoder.encode(payParam.getSn(), java.nio.charset.StandardCharsets.UTF_8);
            }
            AliPayRequest.wapPay(response, payModel, returnUrl,
                    notifyUrl(alipayPaymentSetting.getCallbackUrl(), PaymentMethodEnum.ALIPAY));
        } catch (Exception e) {
            log.error("H5支付异常", e);
            throw new ServiceException(ResultCode.ALIPAY_EXCEPTION);
        }
        return null;
    }


    @Override
    public ResultMessage<Object> jsApiPay(HttpServletRequest request, PayParam payParam) {
        throw new ServiceException(ResultCode.PAY_NOT_SUPPORT);
    }

    @Override
    public ResultMessage<Object> appPay(HttpServletRequest request, PayParam payParam) {
        try {

            CashierParam cashierParam = cashierSupport.cashierParam(payParam);
            AlipayPaymentSetting alipayPaymentSetting = alipayPaymentSetting();
            //请求订单编号
            String outTradeNo = SnowFlake.getIdStr();

            AlipayTradeAppPayModel payModel = new AlipayTradeAppPayModel();

            payModel.setBody(cashierParam.getTitle());
            payModel.setSubject(cashierParam.getDetail());
            payModel.setTotalAmount(cashierParam.getPrice() + "");

            //3分钟超时
            payModel.setTimeoutExpress("3m");
            //回传数据（替换 Hutool 的 URLEncoder 为 JDK 的 java.net.URLEncoder）
            payModel.setPassbackParams(java.net.URLEncoder.encode(BeanUtil.formatKeyValuePair(payParam), "UTF-8"));
            payModel.setOutTradeNo(outTradeNo);
            payModel.setProductCode("QUICK_MSECURITY_PAY");

            log.info("支付宝APP支付：{}", payModel);
            String orderInfo = AliPayRequest.appPayToResponse(payModel, notifyUrl(alipayPaymentSetting.getCallbackUrl(), PaymentMethodEnum.ALIPAY)).getBody();
            updateOrderPayNo(payParam, outTradeNo);
            log.info("支付宝APP支付返回内容：{}", orderInfo);
            return ResultUtil.data(orderInfo);
        } catch (AlipayApiException e) {
            log.error("支付宝支付异常：", e);
            throw new ServiceException(ResultCode.ALIPAY_EXCEPTION);
        } catch (Exception e) {
            log.error("支付业务异常：", e);
            throw new ServiceException(ResultCode.PAY_ERROR);
        }
    }

    @Override
    public ResultMessage<Object> nativePay(HttpServletRequest request, PayParam payParam) {

        try {
            CashierParam cashierParam = cashierSupport.cashierParam(payParam);
            AlipayPaymentSetting alipayPaymentSetting = alipayPaymentSetting();

            AlipayTradePrecreateModel payModel = new AlipayTradePrecreateModel();

            //请求订单编号
            String outTradeNo = SnowFlake.getIdStr();

            payModel.setBody(cashierParam.getTitle());
            payModel.setSubject(cashierParam.getDetail());
            payModel.setTotalAmount(cashierParam.getPrice() + "");

            //回传数据（替换 Hutool 的 URLEncoder 为 JDK 的 java.net.URLEncoder）
            payModel.setPassbackParams(java.net.URLEncoder.encode(BeanUtil.formatKeyValuePair(payParam), "UTF-8"));
            payModel.setTimeoutExpress("3m");
            payModel.setOutTradeNo(outTradeNo);
            log.info("支付宝扫码：{}", payModel);
            String resultStr =
                    AliPayRequest.tradePrecreatePayToResponse(payModel, notifyUrl(alipayPaymentSetting.getCallbackUrl(), PaymentMethodEnum.ALIPAY)).getBody();

            updateOrderPayNo(payParam, outTradeNo);
            log.info("支付宝扫码交互返回：{}", resultStr);
            JSONObject jsonObject = JSONUtil.parseObj(resultStr);
            return ResultUtil.data(jsonObject.getJSONObject("alipay_trade_precreate_response").getStr("qr_code"));
        } catch (Exception e) {
            log.error("支付业务异常：", e);
            throw new ServiceException(ResultCode.PAY_ERROR);
        }
    }


    @Override
    public void refund(RefundLog refundLog) {
        AlipayTradeRefundModel model = new AlipayTradeRefundModel();
        //这里取支付回调时返回的流水
        if (StringUtils.isNotEmpty(refundLog.getPaymentReceivableNo())) {
            model.setTradeNo(refundLog.getPaymentReceivableNo());
        } else {
            throw new ServiceException(ResultCode.ALIPAY_PARAMS_EXCEPTION);
        }
        model.setRefundAmount(refundLog.getTotalAmount() + "");
        model.setRefundReason(refundLog.getRefundReason());
        model.setOutRequestNo(refundLog.getOutOrderNo());
        //交互退款
        try {
            AlipayTradeRefundResponse alipayTradeRefundResponse = AliPayApi.tradeRefundToResponse(model);
            log.info("支付宝退款，参数：{},支付宝响应：{}", JSONUtil.toJsonStr(model), JSONUtil.toJsonStr(alipayTradeRefundResponse));
            if (alipayTradeRefundResponse.isSuccess()) {
                refundLog.setIsRefund(true);
                refundLog.setReceivableNo(refundLog.getOutOrderNo());
                refundLogService.save(refundLog);
            } else {
                String errorMsg = String.format("错误码：%s,错误原因：%s", alipayTradeRefundResponse.getSubCode(),
                        alipayTradeRefundResponse.getSubMsg());
                refundLog.setErrorMessage(errorMsg);
                refundLogService.save(refundLog);
                log.error("支付宝退款业务失败，orderSn={}, errorMsg={}", refundLog.getOrderSn(), errorMsg);
                throw new ServiceException(ResultCode.PAY_ERROR, "支付宝退款失败：" + alipayTradeRefundResponse.getSubMsg());
            }
        } catch (ServiceException e) {
            throw e;
        } catch (Exception e) {
            log.error("支付退款异常：", e);
            throw new ServiceException(ResultCode.PAY_ERROR, "支付宝退款异常：" + (e.getMessage() != null ? e.getMessage() : "请稍后重试"));
        }

    }

    @Override
    public void refundNotify(HttpServletRequest request) {
        //不需要实现
    }

    @Override
    public void callBack(HttpServletRequest request) {
        log.info("支付同步回调：");
        checkPaymentResult(request);

    }

    @Override
    public void notify(HttpServletRequest request) {
        notify(request, ThreadContextHolder.getHttpResponse());
    }

    @Override
    public void notify(HttpServletRequest request, HttpServletResponse response) {
        boolean success = verifyNotify(request);
        log.info("支付宝支付异步通知处理完成，结果：{}", success ? "success" : "fail");
        if (response != null) {
            try {
                response.setContentType("text/plain;charset=UTF-8");
                response.setStatus(HttpServletResponse.SC_OK);
                response.getWriter().write(success ? "success" : "fail");
                response.getWriter().flush();
            } catch (Exception e) {
                log.error("支付宝回调响应写入异常", e);
            }
        }
    }

    /**
     * 支付宝提现
     * 文档地址：https://opendocs.alipay.com/open/02byuo?scene=ca56bca529e64125a2786703c6192d41&ref=api
     *
     * @param memberWithdrawApply 客户提现申请
     */
    @Override
    public TransferResultDTO transfer(MemberWithdrawApply memberWithdrawApply) {
        AlipayFundTransUniTransferModel model = new AlipayFundTransUniTransferModel();
        model.setOutBizNo(SnowFlake.createStr("T"));
        model.setRemark("用户提现");
        model.setBusinessParams("{\"payer_show_name_use_alias\":\"true\"}");
        model.setBizScene("DIRECT_TRANSFER");
        Participant payeeInfo = new Participant();
        payeeInfo.setIdentity(memberWithdrawApply.getConnectNumber());
        payeeInfo.setIdentityType("ALIPAY_LOGON_ID");
        payeeInfo.setName(memberWithdrawApply.getRealName());
        model.setPayeeInfo(payeeInfo);

        model.setTransAmount(memberWithdrawApply.getApplyMoney().toString());
        model.setProductCode("TRANS_ACCOUNT_NO_PWD");
        model.setOrderTitle("用户提现");
        //交互退款
        try {
            AlipayFundTransUniTransferResponse alipayFundTransUniTransferResponse = AliPayApi.uniTransferToResponse(model, null);
            log.error("支付宝退款，参数：{},支付宝响应：{}", JSONUtil.toJsonStr(model), JSONUtil.toJsonStr(alipayFundTransUniTransferResponse));
            if (alipayFundTransUniTransferResponse.isSuccess()) {
                return TransferResultDTO.builder().result(true).build();
            } else {
                log.error(alipayFundTransUniTransferResponse.getSubMsg());
                return TransferResultDTO.builder().result(false).response(alipayFundTransUniTransferResponse.getSubMsg()).build();
            }
        } catch (Exception e) {
            log.error("用户提现异常：", e);
            return TransferResultDTO.builder().result(false).response(e.getMessage()).build();
        }
    }

    /**
     * 验证支付结果
     *
     * @param request 请求
     */
    private void checkPaymentResult(HttpServletRequest request) {
        try {
            AlipayPaymentSetting alipayPaymentSetting = alipayPaymentSetting();
            //获取支付宝反馈信息
            Map<String, String> map = AliPayApi.toMap(request);
            log.info("同步回调：{}", JSONUtil.toJsonStr(map));
            boolean verifyResult = AlipaySignature.rsaCertCheckV1(map, alipayPaymentSetting.getAlipayPublicCertPath(), "UTF-8",
                    "RSA2");
            if (verifyResult) {
                log.info("支付回调通知：支付成功-参数：{}", map);
            } else {
                log.info("支付回调通知：支付失败-参数：{}", map);
            }

            ConnectSetting connectSetting = JSONUtil.toBean(
                    settingService.get(SettingEnum.CONNECT_SETTING.name()).getSettingValue(), ConnectSetting.class);
            String paidMemberSn = request.getParameter("paidMemberSn");
            String target = paidMemberSn != null && paidMemberSn.matches("PM[0-9]+")
                    ? "/pages/mine/paidMember?payment_sn=" + paidMemberSn : "/pages/order/myOrder?status=0";
            ThreadContextHolder.getHttpResponse().sendRedirect(connectSetting.getWap() + target);
        } catch (Exception e) {
            log.error("支付回调同步通知异常", e);
        }

    }

    /**
     * 验证支付结果
     *
     * @param request
     */
    boolean verifyNotify(HttpServletRequest request) {
        try {
            AlipayPaymentSetting alipayPaymentSetting = alipayPaymentSetting();
            //获取支付宝反馈信息
            Map<String, String> map = AliPayApi.toMap(request);
            log.info("支付回调响应：{}", JSONUtil.toJsonStr(map));
            boolean verifyResult = AlipaySignature.rsaCertCheckV1(map, alipayPaymentSetting.getAlipayPublicCertPath(), "UTF-8",
                    "RSA2");
            if (!verifyResult) {
                log.warn("支付回调通知：验签失败-参数：{}", map);
                return false;
            }
            //支付完成判定
            if (!"TRADE_FINISHED".equals(map.get("trade_status")) &&
                    !"TRADE_SUCCESS".equals(map.get("trade_status"))) {
                log.info("支付回调通知：非成功终态 trade_status={}", map.get("trade_status"));
                return false;
            }
            // 校验 app_id 是否匹配本商户配置
            String notifyAppId = map.get("app_id");
            if (CharSequenceUtil.isEmpty(notifyAppId) || !notifyAppId.equals(alipayPaymentSetting.getAppId())) {
                log.error("支付回调通知：app_id 不匹配，通知 appId={}, 系统配置 appId={}", notifyAppId, alipayPaymentSetting.getAppId());
                return false;
            }
            String payParamStr = map.get("passback_params");
            String payParamJson = java.net.URLDecoder.decode(payParamStr, "UTF-8");
            PayParam payParam = BeanUtil.formatKeyValuePair(payParamJson, new PayParam());

            String tradeNo = map.get("trade_no");
            Double totalAmount = Double.parseDouble(map.get("total_amount"));
            PaymentSuccessParams paymentSuccessParams =
                    new PaymentSuccessParams(PaymentMethodEnum.ALIPAY.name(), tradeNo, totalAmount, payParam);

            paymentService.success(paymentSuccessParams);
            log.info("支付回调通知：支付成功-参数：{},回调参数:{}", map, payParam);
            return true;
        } catch (AlipayApiException e) {
            log.error("支付回调通知支付宝API异常", e);
            return false;
        } catch (java.io.UnsupportedEncodingException e) {
            log.error("URL 解码异常", e);
            return false;
        } catch (Exception e) {
            log.error("支付回调通知系统异常", e);
            return false;
        }
    }

    /**
     * 修改订单支付单号与支付方式
     *
     * @param payParam   支付参数
     * @param outOrderNo 第三方订单号
     */
    private void updateOrderPayNo(PayParam payParam, String outOrderNo) {
        if ("ORDER".equals(payParam.getOrderType())) {
            orderService.update(new LambdaUpdateWrapper<Order>()
                    .eq(Order::getSn, payParam.getSn())
                    .set(Order::getPayOrderNo, outOrderNo)
                    .set(Order::getPaymentMethod, PaymentMethodEnum.ALIPAY.name()));
        } else if ("TRADE".equals(payParam.getOrderType())) {
            orderService.update(new LambdaUpdateWrapper<Order>()
                    .eq(Order::getTradeSn, payParam.getSn())
                    .set(Order::getPayOrderNo, outOrderNo)
                    .set(Order::getPaymentMethod, PaymentMethodEnum.ALIPAY.name()));
        }
    }

    /**
     * 主动查询支付宝订单状态
     *
     * @param outTradeNo 外部商户单号（对应 Order.payOrderNo）
     * @return 查单结果
     */
    @Override
    public PaymentQueryResult query(String outTradeNo) {
        if (StringUtils.isEmpty(outTradeNo)) {
            return PaymentQueryResult.builder().exists(false).isPaid(false).isClosed(false).paymentMethod(PaymentMethodEnum.ALIPAY.name()).build();
        }
        try {
            AlipayTradeQueryModel queryModel = new AlipayTradeQueryModel();
            queryModel.setOutTradeNo(outTradeNo);
            AlipayTradeQueryResponse response = AliPayApi.tradeQueryToResponse(queryModel);
            if (response.isSuccess()) {
                String tradeStatus = response.getTradeStatus();
                boolean isPaid = "TRADE_SUCCESS".equals(tradeStatus) || "TRADE_FINISHED".equals(tradeStatus);
                boolean isClosed = "TRADE_CLOSED".equals(tradeStatus);
                Double totalAmount = null;
                if (StringUtils.isNotEmpty(response.getTotalAmount())) {
                    try {
                        totalAmount = Double.parseDouble(response.getTotalAmount());
                    } catch (NumberFormatException ignored) {
                    }
                }
                return PaymentQueryResult.builder()
                        .exists(true)
                        .isPaid(isPaid)
                        .isClosed(isClosed)
                        .paymentMethod(PaymentMethodEnum.ALIPAY.name())
                        .receivableNo(response.getTradeNo())
                        .totalAmount(totalAmount)
                        .rawStatus(tradeStatus)
                        .build();
            } else {
                // 订单不存在
                if ("ACQ.TRADE_NOT_EXIST".equalsIgnoreCase(response.getSubCode())) {
                    log.info("支付宝查单：渠道订单不存在，outTradeNo={}", outTradeNo);
                    return PaymentQueryResult.builder()
                            .exists(false)
                            .isPaid(false)
                            .isClosed(false)
                            .paymentMethod(PaymentMethodEnum.ALIPAY.name())
                            .rawStatus(response.getSubCode())
                            .errorMessage(response.getSubMsg())
                            .build();
                }
                log.error("支付宝查单返回失败：subCode={}, subMsg={}", response.getSubCode(), response.getSubMsg());
                throw new ServiceException("支付宝查单失败: " + response.getSubMsg());
            }
        } catch (AlipayApiException e) {
            log.error("支付宝查单API异常，outTradeNo={}", outTradeNo, e);
            throw new ServiceException("支付宝查单异常: " + e.getErrMsg());
        } catch (ServiceException se) {
            log.warn("支付宝查单业务异常，outTradeNo={}", outTradeNo, se);
            return PaymentQueryResult.builder()
                    .exists(false)
                    .isPaid(false)
                    .isClosed(false)
                    .paymentMethod(PaymentMethodEnum.ALIPAY.name())
                    .errorMessage(se.getMessage())
                    .build();
        } catch (Exception e) {
            log.error("支付宝查单未知异常，outTradeNo={}", outTradeNo, e);
            throw new ServiceException("支付宝查单系统异常: " + e.getMessage());
        }
    }

    /**
     * 关闭支付宝未支付订单
     *
     * @param outTradeNo 外部商户单号（对应 Order.payOrderNo）
     */
    @Override
    public void close(String outTradeNo) {
        if (StringUtils.isEmpty(outTradeNo)) {
            return;
        }
        try {
            AlipayTradeCloseModel model = new AlipayTradeCloseModel();
            model.setOutTradeNo(outTradeNo);
            AlipayTradeCloseResponse response = AliPayApi.tradeCloseToResponse(model);
            if (!response.isSuccess()) {
                if ("ACQ.TRADE_NOT_EXIST".equalsIgnoreCase(response.getSubCode())) {
                    log.info("支付宝关单忽略：订单不存在，outTradeNo={}", outTradeNo);
                    return;
                }
                log.warn("支付宝关单返回未成功：subCode={}, subMsg={}", response.getSubCode(), response.getSubMsg());
            } else {
                log.info("支付宝关单成功，outTradeNo={}", outTradeNo);
            }
        } catch (Exception e) {
            log.warn("支付宝关单异常，outTradeNo={}", outTradeNo, e);
        }
    }

    /**
     * 获取支付宝配置
     *
     * @return
     */
    private AlipayPaymentSetting alipayPaymentSetting() {
        Setting setting = settingService.get(SettingEnum.ALIPAY_PAYMENT.name());
        if (setting != null) {
            return JSONUtil.toBean(setting.getSettingValue(), AlipayPaymentSetting.class);
        }
        throw new ServiceException(ResultCode.ALIPAY_NOT_SETTING);
    }


}
