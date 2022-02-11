package cn.lili.modules.order.cart.entity.dto;

import cn.lili.modules.promotion.entity.dos.MemberCoupon;
import lombok.Data;

import java.io.Serializable;
import java.util.Map;

/**
 * 用于计算优惠券结算详情
 *
 * @author Chopper
 * @since 2020-03-25 2:30 下午
 */
@Data
public class MemberCouponDTO implements Serializable {


    private static final long serialVersionUID = 8276369124551043085L;
    /**
     * 在整比交易中时： key 为店铺id，value 为每个店铺跨店优惠 结算金额
     * 在购物车中时：key为sku id ，value为每个商品结算时的金额
     */
    private Map<String, Double> skuDetail;

    /**
     * 优惠券详情
     */
    private MemberCoupon memberCoupon;

    public MemberCouponDTO(Map<String, Double> skuDetail, MemberCoupon memberCoupon) {
        this.skuDetail = skuDetail;
        this.memberCoupon = memberCoupon;
    }
}
