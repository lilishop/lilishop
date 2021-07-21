package cn.lili.modules.member.entity.vo;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 会员商品收藏VO
 *
 * @author Chopper
 * @since 2021/1/27 10:41 上午
 */
@Data
public class GoodsCollectionVO {

    @ApiModelProperty(value = "id")
    private String id;

    @ApiModelProperty(value = "商品ID")
    private String goodsId;

    @ApiModelProperty(value = "规格ID")
    private String skuId;

    @ApiModelProperty(value = "商品名称")
    private String goodsName;

    @ApiModelProperty(value = "商品图片")
    private String image;

    @ApiModelProperty(value = "商品价格")
    private Double price;

    @ApiModelProperty(value = "已失效")
    private String marketEnable;
}
