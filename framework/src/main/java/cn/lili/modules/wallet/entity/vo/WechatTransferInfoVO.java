package cn.lili.modules.wallet.entity.vo;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 微信转账信息
 *
 * @author lili
 */
@Data
@Schema(description = "微信转账信息")
@NoArgsConstructor
@AllArgsConstructor
public class WechatTransferInfoVO {

    @Schema(description = "微信支付商户号")
    private String mchId;

    @Schema(description = "微信应用ID")
    private String appId;

    @Schema(description = "微信转账 package 信息")
    private String wechatPackage;
}
