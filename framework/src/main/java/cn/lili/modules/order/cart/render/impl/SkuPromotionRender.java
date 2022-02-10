package cn.lili.modules.order.cart.render.impl;

import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.RenderStepEnums;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.cart.render.CartRenderStep;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.promotion.entity.dto.search.KanjiaActivitySearchParams;
import cn.lili.modules.promotion.entity.enums.KanJiaStatusEnum;
import cn.lili.modules.promotion.entity.vos.PromotionSkuVO;
import cn.lili.modules.promotion.entity.vos.kanjia.KanjiaActivityVO;
import cn.lili.modules.promotion.service.KanjiaActivityService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Map;
import java.util.Objects;

/**
 * 购物促销信息渲染实现
 *
 * @author Chopper
 * @since 2020-07-02 14:47
 */
@Service
public class SkuPromotionRender implements CartRenderStep {


    @Autowired
    private KanjiaActivityService kanjiaActivityService;

    @Autowired
    private MemberService memberService;

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
                cartVO -> cartVO.getCheckedSkuList().forEach(cartSkuVO -> {
                    PriceDetailDTO priceDetailDTO = cartSkuVO.getPriceDetailDTO();
                    priceDetailDTO.setGoodsPrice(cartSkuVO.getSubTotal());
                    priceDetailDTO.setDiscountPrice(CurrencyUtil.sub(priceDetailDTO.getOriginalPrice(), cartSkuVO.getSubTotal()));
                })
        );
    }


    /**
     * 渲染单品优惠 积分/拼团/秒杀/砍价
     *
     * @param tradeDTO 购物车视图
     */
    private void renderSkuPromotion(TradeDTO tradeDTO) {


        switch (tradeDTO.getCartTypeEnum()) {

            //这里是双重循环，但是实际积分购买或者是砍价购买时，购物车只有一个商品，所以没有循环操作数据库或者其他的问题
            case POINTS:
                Member userInfo = memberService.getUserInfo();
                long totalPayPoints = 0;
                //处理积分商品购买
                for (CartVO cartVO : tradeDTO.getCartList()) {
                    for (CartSkuVO cartSkuVO : cartVO.getCheckedSkuList()) {
                        cartSkuVO.getPriceDetailDTO().setPayPoint(cartSkuVO.getPoint());
                        PromotionSkuVO promotionSkuVO = new PromotionSkuVO(PromotionTypeEnum.POINTS_GOODS.name(), cartSkuVO.getPointsId());
                        cartSkuVO.getPriceDetailDTO().getJoinPromotion().add(promotionSkuVO);
                        totalPayPoints += cartSkuVO.getPoint();
                    }
                }
                if (userInfo.getPoint() < totalPayPoints) {
                    throw new ServiceException(ResultCode.POINT_NOT_ENOUGH);
                }
                return;
            case KANJIA:
                for (CartVO cartVO : tradeDTO.getCartList()) {
                    for (CartSkuVO cartSkuVO : cartVO.getCheckedSkuList()) {
                        KanjiaActivitySearchParams kanjiaActivitySearchParams = new KanjiaActivitySearchParams();
                        kanjiaActivitySearchParams.setGoodsSkuId(cartSkuVO.getGoodsSku().getId());
                        kanjiaActivitySearchParams.setMemberId(Objects.requireNonNull(UserContext.getCurrentUser()).getId());
                        kanjiaActivitySearchParams.setStatus(KanJiaStatusEnum.SUCCESS.name());
                        KanjiaActivityVO kanjiaActivityVO = kanjiaActivityService.getKanjiaActivityVO(kanjiaActivitySearchParams);
                        //可以砍价金额购买，则处理信息
                        if (Boolean.TRUE.equals(kanjiaActivityVO.getPass())) {
                            cartSkuVO.setKanjiaId(kanjiaActivityVO.getId());
                            cartSkuVO.setPurchasePrice(kanjiaActivityVO.getPurchasePrice());
                            cartSkuVO.setSubTotal(kanjiaActivityVO.getPurchasePrice());
                            cartSkuVO.getPriceDetailDTO().setGoodsPrice(kanjiaActivityVO.getPurchasePrice());
                        }

                        PromotionSkuVO promotionSkuVO = new PromotionSkuVO(PromotionTypeEnum.KANJIA.name(), cartSkuVO.getKanjiaId());
                        cartSkuVO.getPriceDetailDTO().getJoinPromotion().add(promotionSkuVO);
                    }
                }
                return;
            case PINTUAN:
                for (CartVO cartVO : tradeDTO.getCartList()) {
                    for (CartSkuVO cartSkuVO : cartVO.getCheckedSkuList()) {
                        PromotionSkuVO promotionSkuVO = new PromotionSkuVO(PromotionTypeEnum.PINTUAN.name(), cartSkuVO.getPintuanId());
                        cartSkuVO.getPriceDetailDTO().getJoinPromotion().add(promotionSkuVO);
                    }
                }
                return;
            case CART:
            case BUY_NOW:
            case VIRTUAL:
                //循环购物车
                for (CartVO cartVO : tradeDTO.getCartList()) {
                    //循环sku
                    for (CartSkuVO cartSkuVO : cartVO.getCheckedSkuList()) {
                        //赋予商品促销信息
                        for (Map.Entry<String, Object> entry : cartSkuVO.getPromotionMap().entrySet()) {
                            if (ignorePromotion(entry.getKey())) {
                                continue;
                            }

                            JSONObject promotionsObj = JSONUtil.parseObj(entry.getValue());
                            PromotionSkuVO promotionSkuVO = new PromotionSkuVO(entry.getKey().split("-")[0], promotionsObj.get("id", String.class));
                            cartSkuVO.setSubTotal(CurrencyUtil.mul(cartSkuVO.getPurchasePrice(), cartSkuVO.getNum()));
                            cartSkuVO.getPriceDetailDTO().setGoodsPrice(cartSkuVO.getSubTotal());

                            cartSkuVO.getPriceDetailDTO().getJoinPromotion().add(promotionSkuVO);
                        }
                    }
                }
                return;
            default:
        }
    }

    /**
     * 购物车促销类型
     */
    private boolean ignorePromotion(String promotionKey) {

        // 忽略积分活动活动 忽略砍价活动 忽略优惠券活动 忽略拼团活动
        return promotionKey.contains(PromotionTypeEnum.POINTS_GOODS.name())
                || promotionKey.contains(PromotionTypeEnum.KANJIA.name())
                || promotionKey.contains(PromotionTypeEnum.COUPON.name())
                || promotionKey.contains(PromotionTypeEnum.PINTUAN.name());
    }

}
