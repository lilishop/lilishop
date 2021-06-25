package cn.lili.modules.goods.entity.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;

/**
 * 商品参数项
 *
 * @author Chopper
 * @version v1.0
 * 2021-06-24 15:41
 */
@Data
@ApiModel(value = "商品参数列表")
public class GoodsParamsItemDTO {

    @ApiModelProperty(value = "参数ID")
    private String paramId;

    @ApiModelProperty(value = "参数名字")
    private String paramName;

    @ApiModelProperty(value = "参数值")
    private String paramValue;

    @ApiModelProperty(value = "是否可索引，0 不索引 1 索引")
    private Integer isIndex = 0;

    @ApiModelProperty(value = "是否必填，0 不显示 1 显示")
    private Integer required = 0;
}
