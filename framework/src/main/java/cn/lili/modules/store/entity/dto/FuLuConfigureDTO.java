package cn.lili.modules.store.entity.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 店铺福禄配置
 *
 * @author pikachu
 * @since 2021-01-11 15:10:51
 */
@Data
public class FuLuConfigureDTO {

    @ApiModelProperty(value = "福禄app密钥")
    private String appSecretKey;

    @ApiModelProperty(value = "福禄商户号")
    private String merchantNumber;

    @ApiModelProperty(value = "福禄appKEY商户key")
    private String appMerchantKey;
}
