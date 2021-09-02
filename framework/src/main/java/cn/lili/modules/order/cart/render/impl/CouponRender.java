package cn.lili.modules.order.cart.render.impl;

import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.modules.order.cart.entity.dto.MemberCouponDTO;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.RenderStepEnums;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.render.CartRenderStep;
import cn.lili.modules.order.cart.render.util.PromotionPriceUtil;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.promotion.entity.dos.MemberCoupon;
import cn.lili.modules.promotion.entity.enums.CouponScopeTypeEnum;
import cn.lili.modules.promotion.entity.enums.CouponTypeEnum;
import cn.lili.modules.promotion.entity.vos.MemberCouponVO;
import cn.lili.modules.promotion.service.MemberCouponService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

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

    @Autowired
    private MemberCouponService memberCouponService;

    @Override
    public void render(TradeDTO tradeDTO) {

        //优惠券列表
        renderCouponRule(tradeDTO);
        //主要渲染各个优惠的价格
        this.renderCoupon(tradeDTO);
    }


    /**
     * 渲染优惠券规则
     */
    private void renderCouponRule(TradeDTO tradeDTO) {
        List<MemberCoupon> memberCouponList = memberCouponService.getMemberCoupons();

        memberCouponList.forEach(memberCoupon -> {
            available(tradeDTO, memberCoupon);
        });
    }

    /**
     * 判定优惠券是否可用
     *
     * @param tradeDTO
     * @param memberCoupon
     */
    private void available(TradeDTO tradeDTO, MemberCoupon memberCoupon) {
        if (memberCoupon == null) {
            return;
        }
        List<CartSkuVO> filterSku = filterSkuVo(tradeDTO.getSkuList(), memberCoupon);
        if (filterSku == null || filterSku.size() == 0) {
            tradeDTO.getCantUseCoupons().add(new MemberCouponVO(memberCoupon,
                    "购物车中没有满足优惠券使用范围的优惠券"));
            return;
        }
        List<PriceDetailDTO> priceDetailDTOS =
                filterSku.stream().map(CartSkuVO::getPriceDetailDTO).collect(Collectors.toList());

        PriceDetailDTO totalPrice = new PriceDetailDTO();
        totalPrice.accumulationPriceDTO(priceDetailDTOS);

        //满足条件判定
        if (totalPrice.getGoodsPrice() >= memberCoupon.getConsumeThreshold()) {
            tradeDTO.getCanUseCoupons().add(memberCoupon);
        } else {
            tradeDTO.getCantUseCoupons().add(new MemberCouponVO(memberCoupon,
                    "优惠券使用门槛不足，还差" +
                            StringUtils.toFen(CurrencyUtil.sub(memberCoupon.getConsumeThreshold(), totalPrice.getGoodsPrice())) +
                            "元"));
        }

    }

    /**
     * 过滤购物车商品信息，按照优惠券的适用范围过滤
     *
     * @param cartSkuVOS
     * @param memberCoupon
     * @return
     */
    private List<CartSkuVO> filterSkuVo(List<CartSkuVO> cartSkuVOS, MemberCoupon memberCoupon) {

        List<CartSkuVO> filterSku;
        //平台店铺过滤
        if (memberCoupon.getIsPlatform()) {
            filterSku = cartSkuVOS;
        } else {
            filterSku = cartSkuVOS.stream().filter(cartSkuVO -> {
                return cartSkuVO.getStoreId().equals(memberCoupon.getStoreId());
            }).collect(Collectors.toList());
        }
        if (filterSku == null || filterSku.size() == 0) {
            return null;
        }
        //优惠券类型判定
        switch (CouponScopeTypeEnum.valueOf(memberCoupon.getScopeType())) {
            case ALL:
                return filterSku;
            case PORTION_GOODS:
                //按照商品过滤
                filterSku = filterSku.stream().filter(cartSkuVO -> {
                    return memberCoupon.getScopeId().indexOf(cartSkuVO.getGoodsSku().getId()) > 0;
                }).collect(Collectors.toList());
                break;

            case PORTION_SHOP_CATEGORY:
                //按照店铺分类过滤
                filterSku = filterSku.stream().filter(cartSkuVO -> {
                    if (StringUtils.isNotEmpty(cartSkuVO.getGoodsSku().getStoreCategoryPath())) {
                        //获取店铺分类
                        String[] storeCategoryPath = cartSkuVO.getGoodsSku().getStoreCategoryPath().split(",");
                        for (String category : storeCategoryPath) {
                            //店铺分类只要有一项吻合，即可返回true
                            if (memberCoupon.getScopeId().indexOf(category) > 0) {
                                return true;
                            }
                        }
                        return false;
                    } else {
                        return false;
                    }
                }).collect(Collectors.toList());
                break;

            case PORTION_GOODS_CATEGORY:

                //按照店铺分类过滤
                filterSku = filterSku.stream().filter(cartSkuVO -> {
                    //平台分类获取
                    String[] categoryPath = cartSkuVO.getGoodsSku().getCategoryPath().split(",");
                    //平台三级分类
                    String categoryId = categoryPath[categoryPath.length - 1];
                    if (memberCoupon.getScopeId().indexOf(categoryId) > 0) {
                        return true;
                    }
                    return false;
                }).collect(Collectors.toList());
                break;
            default:
                return null;
        }
        return filterSku;
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
            promotionPriceUtil.recountPrice(tradeDTO, memberCouponDTO.getSkuDetail(), memberCouponDTO.getMemberCoupon().getPrice(),
                    coupon.getIsPlatform() ?
                            PromotionTypeEnum.PLATFORM_COUPON : PromotionTypeEnum.COUPON);
            //如果是平台券 则需要计算商家承担比例
            if (coupon.getIsPlatform() && coupon.getStoreCommission() > 0) {

                //循环所有优惠券
                for (String skuId : couponMap.keySet()) {

                    for (CartSkuVO cartSkuVO : tradeDTO.getSkuList()) {
                        //写入平台优惠券承担比例
                        if (cartSkuVO.getGoodsSku().getId().equals(skuId)) {
                            //写入店铺承担比例
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

                        // 打折金额=商品金额*折扣/10
                        Double discountCouponPrice = CurrencyUtil.mul(priceDetailDTO.getGoodsPrice(),
                                CurrencyUtil.sub(1, CurrencyUtil.div(coupon.getDiscount(), 10, 3)));

                        //平台券则写入店铺承担优惠券比例
                        if (coupon.getIsPlatform()) {
                            priceDetailDTO.setSiteCouponPrice(discountCouponPrice);
                            priceDetailDTO.setSiteCouponPoint(coupon.getStoreCommission());
                        }
                        priceDetailDTO.setCouponPrice(CurrencyUtil.add(priceDetailDTO.getCouponPrice(), discountCouponPrice));

                    }
                }
            }
        }

    }
}
