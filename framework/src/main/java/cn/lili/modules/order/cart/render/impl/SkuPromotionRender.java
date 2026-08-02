package cn.lili.modules.order.cart.render.impl;

import cn.lili.cache.Cache;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.modules.goods.service.CardKeyService;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.RenderStepEnums;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.cart.render.CartRenderStep;
import cn.lili.modules.order.order.entity.dto.DiscountPriceItem;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.promotion.entity.dto.search.KanjiaActivitySearchParams;
import cn.lili.modules.promotion.entity.enums.KanJiaStatusEnum;
import cn.lili.modules.promotion.entity.vos.PromotionSkuVO;
import cn.lili.modules.promotion.entity.vos.kanjia.KanjiaActivityVO;
import cn.lili.modules.promotion.service.KanjiaActivityGoodsService;
import cn.lili.modules.promotion.service.KanjiaActivityService;
import cn.lili.modules.promotion.service.PointsGoodsService;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
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
    private KanjiaActivityGoodsService kanjiaActivityGoodsService;

    @Autowired
    private PointsGoodsService pointsGoodsService;

    /**
     * 促销商品
     */
    @Autowired
    private PromotionGoodsService promotionGoodsService;

    @Autowired
    private MemberService memberService;

    @Autowired
    private Cache cache;

    @Autowired
    private CardKeyService cardKeyService;

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
        //检查促销库存
        checkPromotionQuantity(tradeDTO);


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

                        //记录优惠由来
                        cartSkuVO.getPriceDetailDTO().setDiscountPriceItem(
                                DiscountPriceItem.builder()
                                        .discountPrice(CurrencyUtil.sub(cartSkuVO.getGoodsSku().getPrice(), cartSkuVO.getPurchasePrice()))
                                        .promotionId(promotionSkuVO.getActivityId())
                                        .promotionTypeEnum(PromotionTypeEnum.POINTS_GOODS)
                                        .skuId(cartSkuVO.getGoodsSku().getId())
                                        .goodsId(cartSkuVO.getGoodsSku().getGoodsId())
                                        .build()
                        );
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
                        kanjiaActivitySearchParams.setKanjiaActivityId(cartSkuVO.getKanjiaId());
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

                        //记录优惠由来
                        cartSkuVO.getPriceDetailDTO().setDiscountPriceItem(
                                DiscountPriceItem.builder()
                                        .discountPrice(CurrencyUtil.sub(cartSkuVO.getGoodsSku().getPrice(), cartSkuVO.getPurchasePrice()))
                                        .promotionId(promotionSkuVO.getActivityId())
                                        .promotionTypeEnum(PromotionTypeEnum.KANJIA)
                                        .skuId(cartSkuVO.getGoodsSku().getId())
                                        .goodsId(cartSkuVO.getGoodsSku().getGoodsId())
                                        .build()
                        );
                    }
                }
                return;
            case PINTUAN:
                for (CartVO cartVO : tradeDTO.getCartList()) {
                    for (CartSkuVO cartSkuVO : cartVO.getCheckedSkuList()) {
                        PromotionSkuVO promotionSkuVO = new PromotionSkuVO(PromotionTypeEnum.PINTUAN.name(), cartSkuVO.getPintuanId());
                        cartSkuVO.getPriceDetailDTO().getJoinPromotion().add(promotionSkuVO);

                        //记录优惠由来
                        cartSkuVO.getPriceDetailDTO().setDiscountPriceItem(
                                DiscountPriceItem.builder()
                                        .discountPrice(CurrencyUtil.sub(cartSkuVO.getGoodsSku().getPrice(), cartSkuVO.getPurchasePrice()))
                                        .promotionId(promotionSkuVO.getActivityId())
                                        .promotionTypeEnum(PromotionTypeEnum.PINTUAN)
                                        .skuId(cartSkuVO.getGoodsSku().getId())
                                        .goodsId(cartSkuVO.getGoodsSku().getGoodsId())
                                        .build()
                        );
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

                            Object v = entry.getValue();
                            JSONObject promotionsObj;
                            if (v instanceof String) {
                                promotionsObj = JSON.parseObject((String) v);
                            } else if (v instanceof JSONObject) {
                                promotionsObj = (JSONObject) v;
                            } else if (v instanceof Map) {
                                promotionsObj = JSON.parseObject(JSON.toJSONString(v));
                            } else {
                                promotionsObj = JSON.parseObject(JSON.toJSONString(v));
                            }
                            PromotionSkuVO promotionSkuVO = new PromotionSkuVO(entry.getKey().split("-")[0], promotionsObj.getString("id"));
                            cartSkuVO.setSubTotal(CurrencyUtil.mul(cartSkuVO.getPurchasePrice(), cartSkuVO.getNum()));
                            cartSkuVO.getPriceDetailDTO().setGoodsPrice(cartSkuVO.getSubTotal());

                            cartSkuVO.getPriceDetailDTO().getJoinPromotion().add(promotionSkuVO);

                            if (isSkuPricePromotion(promotionSkuVO.getPromotionType())) {
                                PromotionTypeEnum type = PromotionTypeEnum.valueOf(promotionSkuVO.getPromotionType());
                                if (cartSkuVO.getPriceDetailDTO().getDiscountPriceDetail() == null) {
                                    cartSkuVO.getPriceDetailDTO().setDiscountPriceDetail(new ArrayList<>());
                                }
                                DiscountPriceItem item = DiscountPriceItem.builder()
                                        .discountPrice(CurrencyUtil.sub(cartSkuVO.getGoodsSku().getPrice(), cartSkuVO.getPurchasePrice()))
                                        .promotionId(promotionSkuVO.getActivityId())
                                        .promotionTypeEnum(type)
                                        .skuId(cartSkuVO.getGoodsSku().getId())
                                        .goodsId(cartSkuVO.getGoodsSku().getGoodsId())
                                        .build();
                                cartSkuVO.getPriceDetailDTO().getDiscountPriceDetail().add(item);
                                cartSkuVO.getPriceDetailDTO().setDiscountPriceItem(item);
                            }


                        }
                    }
                }
                return;
            default:
        }
    }

    /**
     * 检查促销库存
     *
     * @param tradeDTO 购物车视图
     */
    private void checkPromotionQuantity(TradeDTO tradeDTO) {
        for (CartSkuVO cartSkuVO : tradeDTO.getCheckedSkuList()) {
            List<PromotionSkuVO> joinPromotion = cartSkuVO.getPriceDetailDTO().getJoinPromotion();
            if (!joinPromotion.isEmpty()) {
                for (PromotionSkuVO promotionSkuVO : joinPromotion) {
                    this.checkPromotionGoodsQuantity(cartSkuVO, promotionSkuVO);
                }
            }
        }
    }

    private void checkPromotionGoodsQuantity(CartSkuVO cartSkuVO, PromotionSkuVO promotionSkuVO) {
        String promotionGoodsStockCacheKey = PromotionGoodsService.getPromotionGoodsStockCacheKey(PromotionTypeEnum.valueOf(promotionSkuVO.getPromotionType()), promotionSkuVO.getActivityId(), cartSkuVO.getGoodsSku().getId());
        Object quantity = cache.get(promotionGoodsStockCacheKey);

        if (quantity == null) {
            //如果促销有库存信息
            PromotionTypeEnum promotionTypeEnum = PromotionTypeEnum.valueOf(promotionSkuVO.getPromotionType());
            switch (promotionTypeEnum) {
                case KANJIA:
                    quantity = kanjiaActivityGoodsService.getKanjiaGoodsBySkuId(cartSkuVO.getGoodsSku().getId()).getStock();
                    break;
                case POINTS_GOODS:
                    quantity = pointsGoodsService.getPointsGoodsDetailBySkuId(cartSkuVO.getGoodsSku().getId()).getActiveStock();
                    break;
                case SECKILL:
                case PINTUAN:
                case FLASH_DISCOUNT:
                    quantity = promotionGoodsService.getPromotionGoodsStock(PromotionTypeEnum.valueOf(promotionSkuVO.getPromotionType()), promotionSkuVO.getActivityId(), cartSkuVO.getGoodsSku().getId());
                    break;
                default:
                    return;
            }
        }

        //设置购物车未选中
        if (quantity != null && cartSkuVO.getNum() > (Integer) quantity) {
            cartSkuVO.setChecked(false);
            //设置失效消息
            cartSkuVO.setErrorMessage("促销商品库存不足,现有库存数量[" + quantity + "]");
        }
    }

    private boolean isSkuPricePromotion(String promotionType) {
        return PromotionTypeEnum.SECKILL.name().equals(promotionType)
                || PromotionTypeEnum.FLASH_DISCOUNT.name().equals(promotionType);
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
