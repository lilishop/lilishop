package cn.lili.modules.payment.kit.plugin.unionpay;

import cn.hutool.core.net.URLEncoder;
import cn.hutool.core.util.StrUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.SnowFlake;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.payment.kit.CashierSupport;
import cn.lili.modules.payment.kit.Payment;
import cn.lili.modules.payment.kit.core.enums.SignType;
import cn.lili.modules.payment.kit.core.kit.HttpKit;
import cn.lili.modules.payment.kit.core.kit.IpKit;
import cn.lili.modules.payment.kit.core.kit.WxPayKit;
import cn.lili.modules.payment.kit.dto.PayParam;
import cn.lili.modules.payment.kit.params.dto.CashierParam;
import cn.lili.modules.payment.kit.plugin.unionpay.enums.ServiceEnum;
import cn.lili.modules.payment.kit.plugin.unionpay.model.UnifiedOrderModel;
import cn.lili.modules.payment.service.PaymentService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.payment.UnionPaymentSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.servlet.http.HttpServletRequest;
import java.nio.charset.StandardCharsets;
import java.util.Map;

/**
 * 银联云闪付 支付
 *
 * @author Bulbasaur
 * @since 2023/02/14 17:44
 */
@Slf4j
@Component
public class UnionPayPlugin implements Payment {


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
     * 配置
     */
    @Autowired
    private SettingService settingService;

    @Override
    public ResultMessage<Object> nativePay(HttpServletRequest request, PayParam payParam) {
        try {
            CashierParam cashierParam = cashierSupport.cashierParam(payParam);
            UnionPaymentSetting unionPaymentSetting = this.unionPaymentSetting();
            String notifyUrl = unionPaymentSetting.getUnionPayDomain().concat("/unionPay/payNotify");
            //用户ip
            String ip = IpKit.getRealIp(request);
            //第三方付款订单
            String outOrderNo = SnowFlake.getIdStr();
            String attach = URLEncoder.createDefault().encode(JSONUtil.toJsonStr(payParam), StandardCharsets.UTF_8);
            Map<String, String> params = UnifiedOrderModel.builder()
                    .service(ServiceEnum.NATIVE.toString())
                    .mch_id(unionPaymentSetting.getUnionPayMachId())
                    .out_trade_no(outOrderNo)
                    .body(cashierParam.getDetail())
                    .attach(attach)
                    .total_fee("0")
                    .mch_create_ip(ip)
                    .notify_url(notifyUrl)
                    .nonce_str(WxPayKit.generateStr())
                    .build().createSign(unionPaymentSetting.getUnionPayKey(), SignType.MD5);

            String xmlResult = UnionPayApi.execution(unionPaymentSetting.getUnionPayServerUrl(), params);
            log.info("xmlResult:" + xmlResult);
            Map<String, String> result = WxPayKit.xmlToMap(xmlResult);
            log.info(result.toString());
            return null;
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    @Override
    public ResultMessage<Object> appPay(HttpServletRequest request, PayParam payParam) {
        try {

            CashierParam cashierParam = cashierSupport.cashierParam(payParam);
            UnionPaymentSetting unionPaymentSetting = this.unionPaymentSetting();
            String notifyUrl = unionPaymentSetting.getUnionPayDomain().concat("/unionPay/payNotify");
            String attach = URLEncoder.createDefault().encode(JSONUtil.toJsonStr(payParam), StandardCharsets.UTF_8);

            //用户ip
            String ip = IpKit.getRealIp(request);
            Map<String, String> params = UnifiedOrderModel.builder()
                    .service(ServiceEnum.WEI_XIN_APP_PAY.toString())
                    .mch_id(unionPaymentSetting.getUnionPayMachId())
                    .appid(unionPaymentSetting.getUnionPayAppId())
                    .out_trade_no(WxPayKit.generateStr())
                    .body(cashierParam.getDetail())
                    .attach(attach)
                    .total_fee("0")
                    .mch_create_ip(ip)
                    .notify_url(notifyUrl)
                    .nonce_str(WxPayKit.generateStr())
                    .build()
                    .createSign(unionPaymentSetting.getUnionPayKey(), SignType.MD5);

            System.out.println(params);

            String xmlResult = UnionPayApi.execution(unionPaymentSetting.getUnionPayServerUrl(), params);
            log.info("xmlResult:" + xmlResult);
            Map<String, String> result = WxPayKit.xmlToMap(xmlResult);
            if (!WxPayKit.verifyNotify(result, unionPaymentSetting.getUnionPayKey(), SignType.MD5)) {
                log.error("签名异常");
            }
            String status = result.get("status");
            String resultCode = result.get("result_code");
            if (!"0".equals(status) && !"0".equals(resultCode)) {
                 log.error(result.get("err_msg"));
                return null;
            }
            log.error(result.get("pay_info"));
            return null;
        } catch (Exception e) {
            log.error(e.getMessage());
            e.printStackTrace();

            return null;

        }
    }

    @Override
    public ResultMessage<Object> jsApiPay(HttpServletRequest request, PayParam payParam) {
        String buyerLogonId="";
        String buyerId="";
        CashierParam cashierParam = cashierSupport.cashierParam(payParam);
        UnionPaymentSetting unionPaymentSetting = this.unionPaymentSetting();
        String attach = URLEncoder.createDefault().encode(JSONUtil.toJsonStr(payParam), StandardCharsets.UTF_8);
        //用户ip
        String ip = IpKit.getRealIp(request);
        try {
            if (StrUtil.isEmpty(buyerLogonId) && StrUtil.isEmpty(buyerId)) {
                 log.error("buyer_logon_id buyer_id 不能同时为空");
                return null;
            }

            String notifyUrl = unionPaymentSetting.getUnionPayDomain().concat("/unionPay/payNotify");


            Map<String, String> params = UnifiedOrderModel.builder()
                    .service(ServiceEnum.ALI_PAY_JS_PAY.toString())
                    .mch_id(unionPaymentSetting.getUnionPayMachId())
                    .out_trade_no(WxPayKit.generateStr())
                    .body(cashierParam.getDetail())
                    .attach(attach)
                    .total_fee("0")
                    .mch_create_ip(ip)
                    .notify_url(notifyUrl)
                    .nonce_str(WxPayKit.generateStr())
                    .buyer_id(buyerId)
                    .buyer_logon_id(buyerLogonId)
                    .build()
                    .createSign(unionPaymentSetting.getUnionPayKey(), SignType.MD5);

            System.out.println(params);

            String xmlResult = UnionPayApi.execution(unionPaymentSetting.getUnionPayServerUrl(), params);
            log.info("xmlResult:" + xmlResult);
            Map<String, String> result = WxPayKit.xmlToMap(xmlResult);
            log.info(result.toString());
            return null;
        } catch (Exception e) {
            e.printStackTrace();
            return null;
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

    private void verifyNotify(HttpServletRequest request) throws Exception {
        String xmlMsg = HttpKit.readData(request);
        log.info("支付通知=" + xmlMsg);
        Map<String, String> params = WxPayKit.xmlToMap(xmlMsg);

        String status = params.get("status");
        String returnCode = params.get("result_code");

        log.info(status + " " + returnCode);

        if ("0".equals(status) && "0".equals(returnCode)) {
            UnionPaymentSetting unionPaymentSetting = this.unionPaymentSetting();
            // 注意重复通知的情况，同一订单号可能收到多次通知，请注意一定先判断订单状态
            // 注意此处签名方式需与统一下单的签名类型一致
            if (WxPayKit.verifyNotify(params, unionPaymentSetting.getUnionPayKey(), SignType.MD5)) {
                log.info("支付成功....");
                // 更新订单信息
                // 发送通知等

            }
        }

    }
    /**
     * 获取微信支付配置
     *
     * @return
     */
    private UnionPaymentSetting unionPaymentSetting() {
        try {
            Setting systemSetting = settingService.get(SettingEnum.UNIONPAY_PAYMENT.name());
            UnionPaymentSetting unionPaymentSetting = JSONUtil.toBean(systemSetting.getSettingValue(), UnionPaymentSetting.class);
            return unionPaymentSetting;
        } catch (Exception e) {
            log.error("微信支付暂不支持", e);
            throw new ServiceException(ResultCode.PAY_NOT_SUPPORT);
        }
    }
}
