package cn.lili.modules.store.entity.dto;

import cn.lili.modules.store.entity.dos.StoreDetail;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Size;

/**
 * 店铺修改DTO
 *
 * @author pikachu
 * @date 2020-08-22 15:10:51
 */
@Data
public class StoreEditDTO extends StoreDetail {

    @ApiModelProperty(value = "店铺状态")
    private String storeDisable;

    @ApiModelProperty(value = "是否自营", required = true)
    private Boolean selfOperated;

    @ApiModelProperty(value = "经纬度")
    private String storeCenter;

    @ApiModelProperty(value = "店铺logo")
    private String storeLogo;

    @Size(min = 6, max = 200, message = "店铺简介个数需要在6-200位")
    @NotBlank(message = "店铺简介不能为空")
    @ApiModelProperty(value = "店铺简介")
    private String storeDesc;

    @ApiModelProperty(value = "地址名称， '，'分割")
    private String storeAddressPath;

    @ApiModelProperty(value = "地址id，'，'分割 ")
    private String storeAddressIdPath;

    @ApiModelProperty(value = "详细地址")
    private String storeAddressDetail;

}
