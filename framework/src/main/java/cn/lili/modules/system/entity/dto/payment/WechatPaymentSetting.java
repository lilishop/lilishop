package cn.lili.modules.system.entity.dto.payment;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * 微信支付设置
 *
 * @author Chopper
 * @date 2020-12-02 10:08
 */
@Data
@Accessors(chain = true)
public class WechatPaymentSetting {

    /**
     * 应用id
     */
    private String appId;
    /**
     * 商户号
     */
    private String mchId;
    /**
     * 私钥
     */
    private String apiclient_key;
    /**
     * pem 证书
     */
    private String apiclient_cert_pem;
    /**
     * p12 证书
     */
    private String apiclient_cert_p12;
    /**
     * 商户证书序列号
     */
    private String serialNumber;
    /**
     * apiv3私钥
     */
    private String apiKey3;
}
