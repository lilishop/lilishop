package cn.lili.modules.store.entity.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 店铺发货地址
 *
 * @author chc
 * @since 2022-4-12 10:14:05
 */
@Data
public class StoreDeliverGoodsAddressDTO {

    @ApiModelProperty(value = "发货人姓名")
    private String salesConsignorName;

    @ApiModelProperty(value = "发货人手机号")
    private String salesConsignorMobile;

    @ApiModelProperty(value = "地址Id， '，'分割")
    private String salesConsignorAddressId;

    @ApiModelProperty(value = "地址名称， '，'分割")
    private String salesConsignorAddressPath;

    @ApiModelProperty(value = "详细地址")
    private String salesConsignorDetail;

}
