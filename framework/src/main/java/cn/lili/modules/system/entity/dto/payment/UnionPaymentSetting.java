package cn.lili.modules.system.entity.dto.payment;

import lombok.Data;
import lombok.experimental.Accessors;
/**
 * 银联-云闪付支付设置
 *
 * @author Bulbasaur
 * @since 2023-02-17
 */
@Data
@Accessors(chain = true)
public class UnionPaymentSetting {

    /**
     * 商户号
     */
    private String unionPayMachId;
    /**
     * 密钥
     */
    private String unionPayKey;
    /**
     * 请求地址
     */
    private String unionPayServerUrl;
    /**
     * 交易请求地址
     */
    private String unionPayDomain;
    /**
     * 应用ID
     */
    private String unionPayAppId;
}
