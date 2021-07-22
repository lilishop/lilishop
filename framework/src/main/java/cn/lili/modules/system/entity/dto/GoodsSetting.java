package cn.lili.modules.system.entity.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;

/**
 * 商品设置
 *
 * @author Chopper
 * @since 2020/11/17 7:58 下午
 */
@Data
public class GoodsSetting implements Serializable {

    private static final long serialVersionUID = -4132785717179910025L;
    @ApiModelProperty(value = "是否开启商品审核")
    private Boolean goodsCheck;

    @ApiModelProperty(value = "小图宽")
    private Integer smallPictureWidth;

    @ApiModelProperty(value = "小图高")
    private Integer smallPictureHeight;

    @ApiModelProperty(value = "缩略图宽")
    private Integer abbreviationPictureWidth;

    @ApiModelProperty(value = "缩略图高")
    private Integer abbreviationPictureHeight;

    @ApiModelProperty(value = "原图宽")
    private Integer originalPictureWidth;

    @ApiModelProperty(value = "原图高")
    private Integer originalPictureHeight;

}
