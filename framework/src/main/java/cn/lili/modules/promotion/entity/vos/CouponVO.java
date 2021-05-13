package cn.lili.modules.promotion.entity.vos;

import cn.lili.modules.promotion.entity.dos.Coupon;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.List;

/**
 * 优惠券视图对象
 *
 * @author Chopper
 * @date 2020/8/14
 */
@Data
@ApiModel(value = "优惠券")
public class CouponVO extends Coupon {

    private static final long serialVersionUID = 8372420376262437018L;

    /**
     * 促销关联的商品
     */
    @ApiModelProperty(value = "优惠券关联商品集合")
    private List<PromotionGoods> promotionGoodsList;


}