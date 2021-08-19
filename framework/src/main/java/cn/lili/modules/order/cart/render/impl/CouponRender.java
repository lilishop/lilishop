package cn.lili.modules.order.cart.render.impl;

import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.modules.order.cart.entity.dto.MemberCouponDTO;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.RenderStepEnums;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.render.CartRenderStep;
import cn.lili.modules.order.cart.render.util.PromotionPriceUtil;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.promotion.entity.dos.MemberCoupon;
import cn.lili.modules.promotion.entity.enums.CouponTypeEnum;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Map;

/**
 * 购物促销信息渲染实现
 *
 * @author Chopper
 * @since 2020-07-02 14:47
 */
@Service
public class CouponRender implements CartRenderStep {

    @Override
    public RenderStepEnums step() {
        return RenderStepEnums.COUPON;
    }

    @Autowired
    private PromotionPriceUtil promotionPriceUtil;

    @Override
    public void render(TradeDTO tradeDTO) {
        //主要渲染各个优惠的价格
        this.renderCoupon(tradeDTO);
    }

    /**
     * 渲染优惠券
     *
     * @param tradeDTO 购物车展示信息
     */
    private void renderCoupon(TradeDTO tradeDTO) {
        MemberCouponDTO platformCoupon = tradeDTO.getPlatformCoupon();
        //如果有勾选平台优惠券
        if (platformCoupon != null) {
            renderSku(tradeDTO, platformCoupon);
        }
        //计算商家优惠券
        Map<String, MemberCouponDTO> map = tradeDTO.getStoreCoupons();
        if (map != null && map.size() > 0) {
            for (MemberCouponDTO memberCouponDTO : map.values()) {
                renderSku(tradeDTO, memberCouponDTO);
            }
        }
    }

    /**
     * 渲染sku优惠信息
     *
     * @param tradeDTO        交易DTO
     * @param memberCouponDTO 优惠券DTO
     */
    private void renderSku(TradeDTO tradeDTO, MemberCouponDTO memberCouponDTO) {

        //计算优惠总金额
        Double countPrice = 0D;
        Map<String, Double> couponMap = memberCouponDTO.getSkuDetail();
        for (Double skuPrice : couponMap.values()) {
            countPrice = CurrencyUtil.add(countPrice, skuPrice);
        }

        //接收具体优惠券信息
        MemberCoupon coupon = memberCouponDTO.getMemberCoupon();
        //处理一个极端情况，如果优惠券满减金额大于订单金额
        if (coupon.getCouponType().equals(CouponTypeEnum.PRICE.name()) && coupon.getPrice() > countPrice) {
            //将符合优惠券的金额写入，即最大扣减金额
            coupon.setPrice(countPrice);
        }

        //减免现金，则按照商品价格计算 需要通过工具类进行优惠金额的分发，分发给每个商品
        if (coupon.getCouponType().equals(CouponTypeEnum.PRICE.name())) {
            //分发优惠券
            promotionPriceUtil.recountPrice(tradeDTO, memberCouponDTO.getSkuDetail(), memberCouponDTO.getMemberCoupon().getPrice(), PromotionTypeEnum.COUPON);
            //如果是平台券 则需要计算商家承担比例
            if (coupon.getIsPlatform() && coupon.getStoreCommission() > 0) {

                //循环所有优惠券
                for (String skuId : couponMap.keySet()) {

                    for (CartSkuVO cartSkuVO : tradeDTO.getSkuList()) {
                        //写入平台优惠券承担比例
                        if (cartSkuVO.getGoodsSku().getId().equals(skuId)) {
                            cartSkuVO.getPriceDetailDTO().setSiteCouponPrice(cartSkuVO.getPriceDetailDTO().getSiteCouponPrice());
                            cartSkuVO.getPriceDetailDTO().setSiteCouponPoint(coupon.getStoreCommission());
                        }
                    }
                }
            }
        }
        //打折券 直接计算
        else {
            //循环所有优惠券
            for (String skuId : couponMap.keySet()) {

                // 循环购物车商品
                for (CartSkuVO item : tradeDTO.getSkuList()) {
                    //如果id相等，则渲染商品价格信息
                    if (item.getGoodsSku().getId().equals(skuId)) {

                        PriceDetailDTO priceDetailDTO = item.getPriceDetailDTO();
                        //平台券则写入店铺承担优惠券比例
                        if (coupon.getIsPlatform()) {
                            priceDetailDTO.setSiteCouponPrice(CurrencyUtil.mul(priceDetailDTO.getGoodsPrice(), coupon.getDiscount()));
                            priceDetailDTO.setSiteCouponPoint(coupon.getStoreCommission());
                        }
                        priceDetailDTO.setCouponPrice(CurrencyUtil.add(priceDetailDTO.getCouponPrice(),
                                CurrencyUtil.mul(priceDetailDTO.getGoodsPrice(), coupon.getDiscount())));

                    }
                }
            }
        }

    }
}
