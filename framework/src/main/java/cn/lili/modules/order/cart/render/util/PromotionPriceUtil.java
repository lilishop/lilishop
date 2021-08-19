package cn.lili.modules.order.cart.render.util;

import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.promotion.service.KanjiaActivityGoodsService;
import cn.lili.modules.promotion.service.PointsGoodsService;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import cn.lili.modules.promotion.service.SeckillApplyService;
import cn.lili.modules.search.service.EsGoodsSearchService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * 促销价格计算业务层实现
 *
 * @author paulG
 * @since 2020/8/21
 **/
@Service
@Slf4j
public class PromotionPriceUtil {

    /**
     * ES商品
     */
    @Autowired
    private EsGoodsSearchService goodsSearchService;
    /**
     * 秒杀活动申请
     */
    @Autowired
    private SeckillApplyService seckillApplyService;
    /**
     * 促销商品
     */
    @Autowired
    private PromotionGoodsService promotionGoodsService;
    /**
     * 规格商品
     */
    @Autowired
    private GoodsSkuService goodsSkuService;
    /**
     * 积分商品
     */
    @Autowired
    private PointsGoodsService pointsGoodsService;
    /**
     * 积分商品
     */
    @Autowired
    private KanjiaActivityGoodsService kanjiaActivityGoodsService;


    /**
     * 重新计算购物车价格
     *
     * @param tradeDTO           交易DTO
     * @param skuPromotionDetail 参与活动的商品，以及商品总金额
     * @param discountPrice      需要分发的优惠金额
     * @param promotionTypeEnum  促销类型
     */
    public void recountPrice(TradeDTO tradeDTO, Map<String, Double> skuPromotionDetail, Double discountPrice, PromotionTypeEnum promotionTypeEnum) {

        // sku 促销信息非空判定
        if (skuPromotionDetail == null || skuPromotionDetail.size() == 0) {
            return;
        }

        //计算总金额
        Double totalPrice = 0D;
        for (Double value : skuPromotionDetail.values()) {
            totalPrice = CurrencyUtil.add(totalPrice, value);
        }

        //极端情况，如果扣减金额小于需要支付的金额，则扣减金额=支付金额，不能成为负数
        if (discountPrice > totalPrice) {
            discountPrice = totalPrice;

            for (String skuId : skuPromotionDetail.keySet()) {

                //获取对应商品进行计算
                for (CartSkuVO cartSkuVO : tradeDTO.getSkuList()) {

                    if (cartSkuVO.getGoodsSku().getId().equals(skuId)) {
                        //优惠券金额，则计入优惠券 ，其他则计入总的discount price
                        if (promotionTypeEnum == PromotionTypeEnum.COUPON) {
                            cartSkuVO.getPriceDetailDTO().setCouponPrice(cartSkuVO.getPriceDetailDTO().getGoodsPrice());
                        } else {
                            cartSkuVO.getPriceDetailDTO().setDiscountPrice(cartSkuVO.getPriceDetailDTO().getGoodsPrice());
                        }
                    }
                }
            }
        }

        //获取购物车信息
        List<CartSkuVO> skuVOList = tradeDTO.getSkuList();

        // 获取map分配sku的总数，如果是最后一个商品分配金额，则将金额从百分比改为总金额扣减，避免出现小数除不尽
        Integer count = skuPromotionDetail.size();

        //已优惠金额
        Double deducted = 0D;

        for (String skuId : skuPromotionDetail.keySet()) {

            //获取对应商品进行计算
            for (CartSkuVO cartSkuVO : skuVOList) {

                if (cartSkuVO.getGoodsSku().getId().equals(skuId)) {

                    count--;

                    //sku 优惠金额
                    Double skuDiscountPrice = 0d;

                    //非最后一个商品，则按照比例计算
                    if (count > 0) {
                        //商品金额占比
                        Double point = CurrencyUtil.div(cartSkuVO.getPriceDetailDTO().getGoodsPrice(), totalPrice, 4);
                        //商品优惠金额
                        skuDiscountPrice = CurrencyUtil.mul(discountPrice, point);
                        //累加已优惠金额
                        deducted = CurrencyUtil.add(deducted, skuDiscountPrice);
                    }
                    // 如果是最后一个商品 则减去之前优惠的金额来进行计算
                    else {
                        skuDiscountPrice = CurrencyUtil.sub(discountPrice, deducted);
                    }
                    //优惠券金额，则计入优惠券 ，其他则计入总的discount price
                    if (promotionTypeEnum == PromotionTypeEnum.COUPON) {

                        cartSkuVO.getPriceDetailDTO().setCouponPrice(
                                CurrencyUtil.add(cartSkuVO.getPriceDetailDTO().getCouponPrice(), skuDiscountPrice));
                    } else {
                        cartSkuVO.getPriceDetailDTO().setDiscountPrice(
                                CurrencyUtil.add(cartSkuVO.getPriceDetailDTO().getDiscountPrice(), skuDiscountPrice));
                    }
                }
            }
        }


    }


//    /**
//     * 计算积分商品
//     * 积分商品的购买金额是：0
//     * 1.根据SkuId去查询积分商品（Mongo）
//     * 2.计算积分商品的优惠信息
//     *
//     * @param tradeSkuList 交易商品促销金额列表
//     * @return 计算结果
//     */
//    private List<GoodsSkuPromotionPriceDTO> pointGoodsPromotion(List<PromotionPriceParamDTO> tradeSkuList) {
//        List<GoodsSkuPromotionPriceDTO> priceDTOList = new ArrayList<>();
//        //获取积分商品SkuId
//        String skuId = tradeSkuList.get(0).getSkuId();
//        //获取积分商品VO
//        PointsGoodsVO pointsGoodsVO = pointsGoodsService.getPointsGoodsVOByMongo(skuId);
//        //参与计算的缓存中的商品SKU列表
//        GoodsSku goodsSku = pointsGoodsVO.getGoodsSku();
//        //获取商品促销金额
//        GoodsSkuPromotionPriceDTO goodsSkuPromotionPrice = new GoodsSkuPromotionPriceDTO(goodsSku, tradeSkuList.get(0).getNum());
//        //计算商品原价=原价*数量
//        goodsSkuPromotionPrice.setTotalOriginalPrice(CurrencyUtil.mul(goodsSkuPromotionPrice.getOriginalPrice(), goodsSkuPromotionPrice.getNumber()));
//        //计算商品积分数量=兑换积分*数量
//        goodsSkuPromotionPrice.setTotalPoints(pointsGoodsVO.getPoints() * Convert.toLong(goodsSkuPromotionPrice.getNumber()));
//        //优惠金额=商品原价*数量
//        goodsSkuPromotionPrice.setTotalDiscountPrice(CurrencyUtil.mul(goodsSkuPromotionPrice.getOriginalPrice(), goodsSkuPromotionPrice.getNumber()));
//        //购买价格=积分商品价格为 0
//        goodsSkuPromotionPrice.setTotalFinalePrice(0.0);
//        priceDTOList.add(goodsSkuPromotionPrice);
//        return priceDTOList;
//    }
//
//    /**
//     * 计算砍价商品
//     * 砍价商品只能购买一件
//     * 1.根据SkuId去查询积分商品（Mongo）
//     * 2.计算积分商品的优惠信息
//     *
//     * @param tradeSkuList 交易商品促销金额列表
//     * @return 计算结果
//     */
//    private List<GoodsSkuPromotionPriceDTO> kanjiaPromotion(List<PromotionPriceParamDTO> tradeSkuList) {
//        List<GoodsSkuPromotionPriceDTO> priceDTOList = new ArrayList<>();
//        //获取积分商品SkuId
//        String skuId = tradeSkuList.get(0).getSkuId();
//        //获取积分商品VO
//        KanjiaActivityGoodsDTO kanjiaActivityGoodsDTO = kanjiaActivityGoodsService.getKanJiaGoodsBySku(skuId);
//
//        //参与计算的缓存中的商品SKU列表
//        GoodsSku goodsSku = kanjiaActivityGoodsDTO.getGoodsSku();
//        GoodsSkuPromotionPriceDTO goodsSkuPromotionPrice = new GoodsSkuPromotionPriceDTO(goodsSku, tradeSkuList.get(0).getNum());
//        //优惠金额=商品原价-购买价格
//        goodsSkuPromotionPrice.setTotalDiscountPrice(CurrencyUtil.sub(goodsSkuPromotionPrice.getOriginalPrice(), kanjiaActivityGoodsDTO.getPurchasePrice()));
//        //购买价格=砍价成交金额
//        goodsSkuPromotionPrice.setTotalFinalePrice(kanjiaActivityGoodsDTO.getPurchasePrice());
//        //原价
//        goodsSkuPromotionPrice.setTotalOriginalPrice(goodsSkuPromotionPrice.getOriginalPrice());
//        priceDTOList.add(goodsSkuPromotionPrice);
//        return priceDTOList;
//    }


    /**
     * 检查活动有效时间
     *
     * @param startTime     活动开始时间
     * @param endTime       活动结束时间
     * @param promotionType 活动类型
     * @param promotionId   活动ID
     * @return 是否有效
     */
    private boolean checkPromotionValidTime(Date startTime, Date endTime, String promotionType, String promotionId) {
        long now = System.currentTimeMillis();
        if (startTime.getTime() > now) {
            log.error("商品ID为{}的{}活动开始时间小于当时时间，活动未开始！", promotionId, promotionType);
            return false;
        }
        if (endTime.getTime() < now) {
            log.error("活动ID为{}的{}活动结束时间大于当时时间，活动已结束！", promotionId, promotionType);
            return false;
        }
        return true;
    }
}
