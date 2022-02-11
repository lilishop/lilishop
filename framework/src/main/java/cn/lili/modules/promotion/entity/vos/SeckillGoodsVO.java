package cn.lili.modules.promotion.entity.vos;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;

/**
 * 秒杀活动商品视图对象
 *
 * @author paulG
 * @since 2020/8/26
 **/
@Data
public class SeckillGoodsVO implements Serializable {

    private static final long serialVersionUID = 5170316685407828228L;

    @ApiModelProperty(value = "活动id")
    private String seckillId;

    @ApiModelProperty(value = "时刻")
    private Integer timeLine;

    @ApiModelProperty(value = "商品id")
    private String goodsId;

    @ApiModelProperty(value = "以积分渠道购买需要积分数量")
    private Integer point;

    @ApiModelProperty(value = "skuID")
    private String skuId;

    @ApiModelProperty(value = "商品名称")
    private String goodsName;

    @ApiModelProperty(value = "商品图片")
    private String goodsImage;

    @ApiModelProperty(value = "商家id")
    private String storeId;

    @ApiModelProperty(value = "价格")
    private Double price;

    @ApiModelProperty(value = "促销数量")
    private Integer quantity;

    @ApiModelProperty(value = "已售数量")
    private Integer salesNum;

    @ApiModelProperty(value = "商品原始价格")
    private Double originalPrice;

}
