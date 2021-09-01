package cn.lili.modules.order.cart.render.impl;

import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.RenderStepEnums;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.cart.entity.vo.FullDiscountVO;
import cn.lili.modules.order.cart.render.CartRenderStep;
import cn.lili.modules.order.cart.render.util.PromotionPriceUtil;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.service.FullDiscountService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

/**
 * FullDiscountRender
 *
 * @author Chopper
 * @since 2020-04-01 10:27 上午
 */
@Service
public class FullDiscountRender implements CartRenderStep {

    @Autowired
    private FullDiscountService fullDiscountService;

    @Autowired
    private PromotionPriceUtil promotionPriceUtil;

    @Autowired
    private GoodsSkuService goodsSkuService;

    @Override
    public RenderStepEnums step() {
        return RenderStepEnums.FULL_DISCOUNT;
    }

    @Override
    public void render(TradeDTO tradeDTO) {

        //获取购物车中所有的商品
        List<CartSkuVO> cartSkuList = tradeDTO.getSkuList();

        //店铺集合
        List<CartVO> cartList = tradeDTO.getCartList();


        //店铺id集合
        List<String> storeIds = tradeDTO.getCartList().stream().map(CartVO::getStoreId).collect(Collectors.toList());
        //获取当前店铺进行到满减活动
        List<FullDiscountVO> fullDiscounts = fullDiscountService.currentPromotion(storeIds);
        if (fullDiscounts == null || fullDiscounts.size() == 0) {
            return;
        }

        //循环满减信息
        for (FullDiscountVO fullDiscount : fullDiscounts) {
            //判定参与活动的商品 全品类参与或者部分商品参与，则进行云散
            if (fullDiscount.getNumber() == -1 || fullDiscount.getPromotionGoodsList() != null) {
                //循环店铺购物车
                for (CartVO cart : cartList) {
                    //如果购物车中的店铺id与活动店铺id相等，则进行促销计算
                    if (fullDiscount.getStoreId().equals(cart.getStoreId())) {

                        //如果有赠品，则将赠品信息写入
                        if (fullDiscount.getIsGift()) {
                            GoodsSku goodsSku = goodsSkuService.getGoodsSkuByIdFromCache(fullDiscount.getGiftId());
                            fullDiscount.setGiftSku(goodsSku);
                        }

                        //写入满减活动
                        cart.setFullDiscount(fullDiscount);
                        Map<String, Double> skuPriceDetail;
                        //参与活动的sku判定
                        skuPriceDetail = initFullDiscountGoods(fullDiscount, cartSkuList);
                        //记录参与满减活动的sku
                        cart.setFullDiscountSkuIds(new ArrayList<>(skuPriceDetail.keySet()));

                        Double countPrice = countPrice(skuPriceDetail);


                        if (isFull(countPrice, cart)) {
                            //如果减现金
                            if (fullDiscount.getIsFullMinus()) {
                                promotionPriceUtil.recountPrice(tradeDTO, skuPriceDetail, fullDiscount.getFullMinus(), PromotionTypeEnum.FULL_DISCOUNT);
                            }
                            //打折
                            else if (fullDiscount.getIsFullRate()) {
                                this.renderFullRate(cart, skuPriceDetail, CurrencyUtil.div(fullDiscount.getFullRate(), 10));
                            }
                            //渲染满优惠
                            renderFullMinus(cart);
                        }
                    }

                }
            }
        }

    }

    /**
     * 渲染满折
     *
     * @param cart
     * @param skuPriceDetail
     */
    private void renderFullRate(CartVO cart, Map<String, Double> skuPriceDetail, Double rate) {

        List<CartSkuVO> cartSkuVOS = cart.getSkuList().stream().filter(cartSkuVO -> {
            return skuPriceDetail.containsKey(cartSkuVO.getGoodsSku().getId());
        }).collect(Collectors.toList());

        // 循环计算扣减金额
        cartSkuVOS.forEach(cartSkuVO -> {
            PriceDetailDTO priceDetailDTO = cartSkuVO.getPriceDetailDTO();

            //优惠金额=旧的优惠金额+商品金额*商品折扣比例
            priceDetailDTO.setDiscountPrice(
                    CurrencyUtil.add(priceDetailDTO.getDiscountPrice(),
                            CurrencyUtil.mul(priceDetailDTO.getGoodsPrice(),
                                    CurrencyUtil.sub(1, rate)
                            )
                    )
            );

        });

    }

    /**
     * 获取参与满优惠的商品id
     *
     * @param fullDiscount 满优惠信息
     * @param cartSkuVOS   购物车商品sku信息
     * @return 参与满优惠的商品id
     */
    public Map<String, Double> initFullDiscountGoods(FullDiscountVO fullDiscount, List<CartSkuVO> cartSkuVOS) {
        Map<String, Double> skuPriceDetail = new HashMap<>(16);

        //全品类参与
        if (fullDiscount.getNumber() == -1) {
            for (CartSkuVO cartSkuVO : cartSkuVOS) {
                skuPriceDetail.put(cartSkuVO.getGoodsSku().getId(), cartSkuVO.getPriceDetailDTO().getGoodsPrice());
            }
        } else {
            //判定参与活动的商品
            for (PromotionGoods promotionGoods : fullDiscount.getPromotionGoodsList()) {
                //sku 集合判定
                for (CartSkuVO cartSkuVO : cartSkuVOS) {
                    // 如果参加满减，并且购物车选中状态 ，则记录商品sku
                    if (cartSkuVO.getChecked() && cartSkuVO.getGoodsSku().getId().equals(promotionGoods.getSkuId())) {
                        skuPriceDetail.put(cartSkuVO.getGoodsSku().getId(), cartSkuVO.getPriceDetailDTO().getGoodsPrice());
                    }
                }
            }
        }
        return skuPriceDetail;
    }

    /**
     * 渲染满减优惠
     *
     * @param cartVO 购物车满优惠渲染
     */
    private void renderFullMinus(CartVO cartVO) {
        //获取参与活动的商品总价
        FullDiscountVO fullDiscount = cartVO.getFullDiscount();

        if (fullDiscount.getIsCoupon()) {
            cartVO.getGiftCouponList().add(fullDiscount.getCouponId());
        }
        if (fullDiscount.getIsGift()) {
            cartVO.setGiftList(Arrays.asList(fullDiscount.getGiftId().split(",")));
        }
        if (fullDiscount.getIsPoint()) {
            cartVO.setGiftPoint(fullDiscount.getPoint());
        }
        //如果满足，判定是否免邮，免邮的话需要渲染一边sku
        if (fullDiscount.getIsFreeFreight()) {
            for (CartSkuVO skuVO : cartVO.getSkuList()) {
                skuVO.setIsFreeFreight(true);
            }
        }
    }


    /**
     * 是否满足满优惠
     *
     * @param cart 购物车展示信息
     * @return 是否满足满优惠
     */
    private boolean isFull(Double price, CartVO cart) {
        if (cart.getFullDiscount().getFullMoney() <= price) {
            cart.setPromotionNotice("正在参与满优惠活动[" + cart.getFullDiscount().getPromotionName() + "]" + cart.getFullDiscount().notice());
            return true;
        } else {
            cart.setPromotionNotice("还差" + CurrencyUtil.sub(cart.getFullDiscount().getFullMoney(), price) + " 即可参与活动（" + cart.getFullDiscount().getPromotionName() + "）" + cart.getFullDiscount().notice());
            return false;
        }
    }

    /**
     * 统计参与满减商品价格
     *
     * @param skuPriceMap sku价格
     * @return 总价
     */
    private Double countPrice(Map<String, Double> skuPriceMap) {
        Double count = 0d;

        for (Double price : skuPriceMap.values()) {
            count = CurrencyUtil.add(count, price);
        }

        return count;
    }
}
