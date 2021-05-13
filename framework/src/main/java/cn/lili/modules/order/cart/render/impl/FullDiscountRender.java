package cn.lili.modules.order.cart.render.impl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.DeliveryMethodEnum;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.service.FullDiscountService;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.cart.entity.vo.FullDiscountVO;
import cn.lili.modules.order.cart.render.CartRenderStep;
import lombok.RequiredArgsConstructor;
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

        // 获取购物车中所有的商品
        List<CartSkuVO> cartSkuList = tradeDTO.getSkuList();

        // 渲染的购物车
        List<CartVO> cartList = new ArrayList<>();

        // 确定有哪些商家
        List<String> storeIds = new ArrayList<>();

        // 根据店铺分组
        Map<String, List<CartSkuVO>> storeCollect = cartSkuList.parallelStream().collect(Collectors.groupingBy(CartSkuVO::getStoreId));
        for (Map.Entry<String, List<CartSkuVO>> storeCart : storeCollect.entrySet()) {
            if (!storeCart.getValue().isEmpty()) {
                storeIds.add(storeCart.getKey());
                CartVO cartVO = new CartVO(storeCart.getValue().get(0));
                if (CharSequenceUtil.isEmpty(cartVO.getDeliveryMethod())) {
                    cartVO.setDeliveryMethod(DeliveryMethodEnum.LOGISTICS.name());
                }
                cartVO.setSkuList(storeCart.getValue());
                storeCart.getValue().stream().filter(i -> Boolean.TRUE.equals(i.getChecked())).findFirst().ifPresent(cartSkuVO -> cartVO.setChecked(true));
                cartList.add(cartVO);

            }
        }

        List<FullDiscountVO> fullDiscounts = fullDiscountService.currentPromotion(storeIds);
        for (FullDiscountVO fullDiscount : fullDiscounts) {
            if (fullDiscount.getPromotionGoodsList() != null || fullDiscount.getNumber() == -1) {
                for (CartVO cart : cartList) {
                    if (fullDiscount.getStoreId().equals(cart.getStoreId())) {
                        cart.setFullDiscount(fullDiscount);
                        List<String> skuIds;
                        if (fullDiscount.getNumber() != -1) {
                            skuIds = initFullDiscountGoods(fullDiscount, cartSkuList);
                        } else {
                            skuIds = cart.getSkuList().stream().map(i -> i.getGoodsSku().getId()).collect(Collectors.toList());
                        }
                        cart.setFullDiscountSkuIds(skuIds);
                    }
                }
            }
        }

        tradeDTO.setCartList(cartList);
    }

    /**
     * 获取参与满优惠的商品id
     *
     * @param fullDiscount 满优惠信息
     * @param cartSkuVOS 购物车商品sku信息
     * @return 参与满优惠的商品id
     */
    public List<String> initFullDiscountGoods(FullDiscountVO fullDiscount, List<CartSkuVO> cartSkuVOS) {
        List<String> goodsIds = new ArrayList<>();
        for (PromotionGoods promotionGoods : fullDiscount.getPromotionGoodsList()) {
            for (CartSkuVO cartSkuVO : cartSkuVOS) {
                if (cartSkuVO.getGoodsSku().getId().equals(promotionGoods.getSkuId())) {
                    goodsIds.add(promotionGoods.getSkuId());
                }
            }
        }
        return goodsIds;
    }

}
