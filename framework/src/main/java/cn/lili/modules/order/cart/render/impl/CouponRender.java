package cn.lili.modules.order.cart.render.impl;

import cn.lili.common.utils.CurrencyUtil;
import cn.lili.modules.order.cart.entity.dto.MemberCouponDTO;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.render.CartRenderStep;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Service;

import java.util.Map;

/**
 * 购物促销信息渲染实现
 *
 * @author Chopper
 * @date 2020-07-02 14:47
 */
@Order(3)
@Service
public class CouponRender implements CartRenderStep {

    @Override
    public void render(TradeDTO tradeDTO) {

        //主要渲染各个优惠的价格
        //this.renderCoupon(tradeDTO);

    }

    /**
     * 渲染优惠券
     *
     * @param tradeDTO 购物车展示信息
     */
    private void renderCoupon(TradeDTO tradeDTO) {
        MemberCouponDTO platformCoupon = tradeDTO.getPlatformCoupon();
        if (platformCoupon != null) {
            //计算平台优惠券
            for (CartSkuVO item : tradeDTO.getSkuList()) {
                //如果这个sku涉及到平台优惠券价格计算 则操作这个sku的价格
                if (platformCoupon.getSkuDetail().containsKey(item.getGoodsSku().getId())) {
                    PriceDetailDTO priceDetailDTO = item.getPriceDetailDTO();
                    priceDetailDTO.setSiteCouponPoint(platformCoupon.getMemberCoupon().getStoreCommission());
                    priceDetailDTO.setSiteCouponPrice(platformCoupon.getSkuDetail().get(item.getGoodsSku().getId()));
                    priceDetailDTO.setSiteCouponCommission(CurrencyUtil.mul(priceDetailDTO.getSiteCouponPoint(), priceDetailDTO.getSiteCouponPrice()));
                }
            }
        }

        //计算商家优惠券
        Map<String, MemberCouponDTO> map = tradeDTO.getStoreCoupons();
        if (map != null && map.size() > 0) {
            for (MemberCouponDTO coupon : map.values()) {
                for (CartSkuVO item : tradeDTO.getSkuList()) {
                    if (coupon.getSkuDetail().containsKey(item.getGoodsSku().getId())) {
                        PriceDetailDTO priceDetailDTO = item.getPriceDetailDTO();
                        //写入商品折扣金额
                        priceDetailDTO.setDiscountPrice(
                                CurrencyUtil.add(
                                        priceDetailDTO.getDiscountPrice(),
                                        coupon.getSkuDetail().get(
                                                item.getGoodsSku().getId()
                                        )
                                )
                        );
                    }
                }
            }
        }
    }


}
