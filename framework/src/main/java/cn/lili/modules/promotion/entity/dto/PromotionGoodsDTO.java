package cn.lili.modules.promotion.entity.dto;

import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;


/**
 * 促销商品数据传输对象
 *
 * @author paulG
 * @date 2020/10/9
 **/
@Data
@NoArgsConstructor
public class PromotionGoodsDTO extends PromotionGoods {

    private static final long serialVersionUID = 9206970681612883421L;

    @ApiModelProperty(value = "原价")
    private Double originPrice;

    @ApiModelProperty(value = "商品id")
    private String goodsId;

    @ApiModelProperty(value = "商品名称")
    private String goodsName;

    @ApiModelProperty(value = "商品图片")
    private String goodsImage;

    public PromotionGoodsDTO(GoodsSku sku) {
        super(sku);
    }
}
