package cn.lili.modules.order.cart.render.impl;

import cn.lili.common.utils.CurrencyUtil;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.order.cart.entity.enums.RenderStepEnums;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.cart.render.CartRenderStep;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * 购物促销信息渲染实现
 *
 * @author Chopper
 * @since 2020-07-02 14:47
 */
@Service
public class SkuPromotionRender implements CartRenderStep {


    /**
     * 促销商品
     */
    @Autowired
    private PromotionGoodsService promotionGoodsService;

    @Override
    public RenderStepEnums step() {
        return RenderStepEnums.SKU_PROMOTION;
    }

    @Override
    public void render(TradeDTO tradeDTO) {

        //基础价格渲染
        renderBasePrice(tradeDTO);
        //渲染单品促销
        renderSkuPromotion(tradeDTO);
    }

    /**
     * 基础价格渲染
     *
     * @param tradeDTO
     */
    private void renderBasePrice(TradeDTO tradeDTO) {
        tradeDTO.getCartList().forEach(
                cartVO -> {
                    cartVO.getSkuList().forEach(cartSkuVO -> {
                        PriceDetailDTO priceDetailDTO = cartSkuVO.getPriceDetailDTO();
                        priceDetailDTO.setOriginalPrice(CurrencyUtil.mul(cartSkuVO.getGoodsSku().getPrice(), cartSkuVO.getNum()));
                        priceDetailDTO.setGoodsPrice(cartSkuVO.getSubTotal());
                        priceDetailDTO.setDiscountPrice(CurrencyUtil.sub(priceDetailDTO.getOriginalPrice(), cartSkuVO.getSubTotal()));
                    });
                }
        );
    }


    /**
     * 渲染单品优惠 积分/拼团/秒杀/砍价
     *
     * @param tradeDTO 购物车视图
     */
    private void renderSkuPromotion(TradeDTO tradeDTO) {

        //非积分商品、拼团、砍价商品可渲染满优惠活动
        //这里普通购物车也只渲染满优惠，其他优惠都是商品级别的，都写在商品属性里
        if (!tradeDTO.getCartTypeEnum().equals(CartTypeEnum.POINTS)
                && !tradeDTO.getCartTypeEnum().equals(CartTypeEnum.PINTUAN)
                && !tradeDTO.getCartTypeEnum().equals(CartTypeEnum.KANJIA)) {
            for (CartVO cartVO : tradeDTO.getCartList()) {
                for (CartSkuVO cartSkuVO : cartVO.getSkuList()) {
                    promotionGoodsService.updatePromotion(cartSkuVO);
                }
            }
        }
    }


}
