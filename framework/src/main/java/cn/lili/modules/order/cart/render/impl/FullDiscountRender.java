package cn.lili.modules.order.cart.render.impl;

import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.cart.entity.vo.FullDiscountVO;
import cn.lili.modules.order.cart.render.CartRenderStep;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.service.FullDiscountService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * FullDiscountRender
 *
 * @author Chopper
 * @date 2020-04-01 10:27 上午
 */
@Service
@Order(1)
public class FullDiscountRender implements CartRenderStep {

    @Autowired
    private FullDiscountService fullDiscountService;

    @Override
    public void render(TradeDTO tradeDTO) {

        //获取购物车中所有的商品
        List<CartSkuVO> cartSkuList = tradeDTO.getSkuList();

        //店铺id集合
        List<String> storeIds = new ArrayList<>();

        //店铺集合
        List<CartVO> cartList = tradeDTO.getCartList();


        //获取店铺id
        Map<String, List<CartSkuVO>> storeCollect = tradeDTO.getSkuList().parallelStream().collect(Collectors.groupingBy(CartSkuVO::getStoreId));
        for (Map.Entry<String, List<CartSkuVO>> storeCart : storeCollect.entrySet()) {
            if (!storeCart.getValue().isEmpty()) {
                storeIds.add(storeCart.getKey());
            }
        }

        //获取当前店铺进行到满减活动
        List<FullDiscountVO> fullDiscounts = fullDiscountService.currentPromotion(storeIds);
        //循环满减信息
        for (FullDiscountVO fullDiscount : fullDiscounts) {
            //判定参与活动的商品
            if (fullDiscount.getPromotionGoodsList() != null || fullDiscount.getNumber() == -1) {
                //循环店铺购物车
                for (CartVO cart : cartList) {
                    //如果购物车中的店铺id与活动店铺id相等，则进行促销计算
                    if (fullDiscount.getStoreId().equals(cart.getStoreId())) {
                        //写入满减活动
                        cart.setFullDiscount(fullDiscount);
                        List<String> skuIds;
                        //参与活动的sku判定
                        if (fullDiscount.getNumber() != -1) {
                            skuIds = initFullDiscountGoods(fullDiscount, cartSkuList);
                        } else {
                            skuIds = cart.getSkuList().stream().map(i -> i.getGoodsSku().getId()).collect(Collectors.toList());
                        }
                        //记录参与满减活动的sku
                        cart.setFullDiscountSkuIds(skuIds);
                    }
                }
            }
        }

    }

    /**
     * 获取参与满优惠的商品id
     *
     * @param fullDiscount 满优惠信息
     * @param cartSkuVOS   购物车商品sku信息
     * @return 参与满优惠的商品id
     */
    public List<String> initFullDiscountGoods(FullDiscountVO fullDiscount, List<CartSkuVO> cartSkuVOS) {
        List<String> goodsIds = new ArrayList<>();
        //判定参与活动的商品
        for (PromotionGoods promotionGoods : fullDiscount.getPromotionGoodsList()) {
            //sku 集合判定
            for (CartSkuVO cartSkuVO : cartSkuVOS) {
                //如果参加，则记录商品sku
                if (cartSkuVO.getGoodsSku().getId().equals(promotionGoods.getSkuId())) {
                    goodsIds.add(promotionGoods.getSkuId());
                }
            }
        }
        return goodsIds;
    }

}
