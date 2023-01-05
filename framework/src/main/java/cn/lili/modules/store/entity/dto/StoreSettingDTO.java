package cn.lili.modules.store.entity.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.NotEmpty;

/**
 * 店铺设置
 *
 * @author Bulbasaur
 * @since 2020/12/16 15:15
 */
@Data
public class StoreSettingDTO {

    @ApiModelProperty(value = "店铺logo")
    private String storeLogo;

    @ApiModelProperty(value = "店铺简介")
    private String storeDesc;

    @ApiModelProperty(value = "地址id，'，'分割 ")
    private String storeAddressIdPath;

    @ApiModelProperty(value = "地址名称， '，'分割")
    private String storeAddressPath;

    @ApiModelProperty(value = "详细地址")
    private String storeAddressDetail;

    @NotEmpty
    @ApiModelProperty(value = "经纬度")
    private String storeCenter;

    @ApiModelProperty(value = "默认页面是否开启")
    private Boolean pageShow;

}
