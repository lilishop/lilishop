package cn.lili.modules.store.entity.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 店铺售后收件地址
 *
 * @author pikachu
 * @since 2020-08-22 15:10:51
 */
@Data
public class StoreAfterSaleAddressDTO {

    @ApiModelProperty(value = "收货人姓名")
    private String salesConsigneeName;

    @ApiModelProperty(value = "收件人手机")
    private String salesConsigneeMobile;

    @ApiModelProperty(value = "地址Id， '，'分割")
    private String salesConsigneeAddressId;

    @ApiModelProperty(value = "地址名称， '，'分割")
    private String salesConsigneeAddressPath;

    @ApiModelProperty(value = "详细地址")
    private String salesConsigneeDetail;
}
