package cn.lili.modules.store.entity.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.Size;

/**
 * 店铺入驻其他信息
 *
 * @author Bulbasaur
 * @since 2020/12/7 16:16
 */
@Data
public class StoreOtherInfoDTO {

    @Size(min = 2, max = 200)
    @NotBlank(message = "店铺名称不能为空")
    @ApiModelProperty(value = "店铺名称")
    private String storeName;

    @ApiModelProperty(value = "店铺logo")
    private String storeLogo;

    @Size(min = 6, max = 200)
    @NotBlank(message = "店铺简介不能为空")
    @ApiModelProperty(value = "店铺简介")
    private String storeDesc;

    @ApiModelProperty(value = "经纬度")
    @NotEmpty
    private String storeCenter;

    @NotBlank(message = "店铺经营类目不能为空")
    @ApiModelProperty(value = "店铺经营类目")
    private String goodsManagementCategory;

    @NotBlank(message = "地址不能为空")
    @ApiModelProperty(value = "地址名称， '，'分割")
    private String storeAddressPath;

    @NotBlank(message = "地址ID不能为空")
    @ApiModelProperty(value = "地址id，'，'分割 ")
    private String storeAddressIdPath;

    @NotBlank(message = "地址详情")
    @ApiModelProperty(value = "地址详情")
    private String storeAddressDetail;

}
