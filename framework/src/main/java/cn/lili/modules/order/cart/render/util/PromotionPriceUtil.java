package cn.lili.modules.order.cart.render.util;

import cn.hutool.core.map.MapUtil;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.order.entity.dto.DiscountPriceItem;
import lombok.extern.slf4j.Slf4j;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

/**
 * 促销价格计算业务层实现
 *
 * @author paulG
 * @since 2020/8/21
 **/
@Slf4j
public class PromotionPriceUtil {

    /**
     * 重新计算购物车价格
     *
     * @param tradeDTO           交易DTO
     * @param skuPromotionDetail 参与活动的商品，以及商品总金额
     * @param discountPrice      需要分发的优惠金额
     * @param promotionTypeEnum  促销类型
     */
    public static void recountPrice(TradeDTO tradeDTO, Map<String, Double> skuPromotionDetail, Double discountPrice,
                                    PromotionTypeEnum promotionTypeEnum, String activityId) {

        // sku 促销信息非空判定
        if (skuPromotionDetail == null || skuPromotionDetail.isEmpty()) {
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
        AtomicInteger count = new AtomicInteger(skuPromotionDetail.size());

        //已优惠金额
        AtomicReference<Double> deducted = new AtomicReference<>(0D);

        for (String skuId : skuPromotionDetail.keySet()) {

            //获取对应商品进行计算

            Double finalDiscountPrice = discountPrice;
            Double finalTotalPrice = totalPrice;
            skuVOList.stream().filter(l -> l.getGoodsSku().getId().equals(skuId)).findFirst().ifPresent(cartSkuVO -> {
                //sku 优惠金额
                Double skuDiscountPrice;
                count.getAndDecrement();

                //非最后一个商品，则按照比例计算
                if (count.get() > 0) {
                    //商品金额占比
                    double point = CurrencyUtil.div(cartSkuVO.getPriceDetailDTO().getGoodsPrice(), finalTotalPrice, 4);
                    //商品优惠金额
                    skuDiscountPrice = CurrencyUtil.mul(finalDiscountPrice, point);
                    //累加已优惠金额
                    deducted.set(CurrencyUtil.add(deducted.get(), skuDiscountPrice));
                }
                // 如果是最后一个商品 则减去之前优惠的金额来进行计算
                else {
                    skuDiscountPrice = CurrencyUtil.sub(finalDiscountPrice, deducted.get());
                }

                calculateCartSkuPromotionsPrice(cartSkuVO, skuDiscountPrice, promotionTypeEnum, activityId);
            });

        }

        calculateNotEnoughPromotionsPrice(skuVOList, skuPromotionDetail, discountPrice, totalPrice, promotionTypeEnum, activityId);

    }


    /**
     * 计算购物车商品优惠金额
     *
     * @param cartSkuVO         购物车商品
     * @param skuDiscountPrice  商品优惠金额
     * @param promotionTypeEnum 优惠类型
     * @param activityId        优惠活动id
     */
    private static void calculateCartSkuPromotionsPrice(CartSkuVO cartSkuVO, Double skuDiscountPrice, PromotionTypeEnum promotionTypeEnum,
                                                        String activityId) {
        //优惠券金额，则计入优惠券 ，其他则计入总的discount price
        if (promotionTypeEnum == PromotionTypeEnum.COUPON) {

            cartSkuVO.getPriceDetailDTO().setCouponPrice(
                    CurrencyUtil.add(cartSkuVO.getPriceDetailDTO().getCouponPrice(), skuDiscountPrice));

            cartSkuVO.getPriceDetailDTO().addDiscountPriceItem(
                    DiscountPriceItem.builder()
                            .goodsId(cartSkuVO.getGoodsSku().getGoodsId())
                            .skuId(cartSkuVO.getGoodsSku().getId())
                            .discountPrice(skuDiscountPrice)
                            .promotionTypeEnum(PromotionTypeEnum.COUPON)
                            .promotionId(activityId)
                            .build()
            );

        } else if (promotionTypeEnum == PromotionTypeEnum.PLATFORM_COUPON) {

            cartSkuVO.getPriceDetailDTO().setSiteCouponPrice(
                    CurrencyUtil.add(cartSkuVO.getPriceDetailDTO().getCouponPrice(), skuDiscountPrice));

            cartSkuVO.getPriceDetailDTO().setCouponPrice(
                    CurrencyUtil.add(cartSkuVO.getPriceDetailDTO().getCouponPrice(), cartSkuVO.getPriceDetailDTO().getSiteCouponPrice()));


            cartSkuVO.getPriceDetailDTO().addDiscountPriceItem(
                    DiscountPriceItem.builder()
                            .goodsId(cartSkuVO.getGoodsSku().getGoodsId())
                            .skuId(cartSkuVO.getGoodsSku().getId())
                            .discountPrice(skuDiscountPrice)
                            .promotionTypeEnum(PromotionTypeEnum.PLATFORM_COUPON)
                            .promotionId(activityId)
                            .build()
            );

        } else {
            cartSkuVO.getPriceDetailDTO().setDiscountPrice(
                    CurrencyUtil.add(cartSkuVO.getPriceDetailDTO().getDiscountPrice(), skuDiscountPrice));

            //目前剩余的只有满减金额活动。后续如果需要调整，这里建议传递活动类型进来
            cartSkuVO.getPriceDetailDTO().addDiscountPriceItem(
                    DiscountPriceItem.builder()
                            .goodsId(cartSkuVO.getGoodsSku().getGoodsId())
                            .skuId(cartSkuVO.getGoodsSku().getId())
                            .discountPrice(skuDiscountPrice)
                            .promotionTypeEnum(PromotionTypeEnum.FULL_DISCOUNT)
                            .promotionId(activityId)
                            .build()
            );
        }
    }

    /**
     * 特殊情况处理，如参与多个促销活动，部分商品在其他促销计算后的金额不足以满足与当前参与的促销活动的优惠金额（只计算使用优惠券不足的情况）
     *
     * @param skuVOList          获取购物车信息
     * @param skuPromotionDetail 参与活动的商品，以及商品总金额
     * @param discountPrice      需要分发的优惠金额
     * @param totalPrice         计算总金额
     * @param promotionTypeEnum  优惠类型
     * @param activityId         优惠活动id
     */
    private static void calculateNotEnoughPromotionsPrice(List<CartSkuVO> skuVOList, Map<String, Double> skuPromotionDetail, Double discountPrice,
                                                          Double totalPrice, PromotionTypeEnum promotionTypeEnum, String activityId) {
        // 特殊情况处理，如参与多个促销活动，部分商品在其他促销计算后的金额不足以满足与当前参与的促销活动的优惠金额
        // 但当前购物车内存在当前当前促销活动的其他商品且剩余金额也满足分摊不足商品的不足金额，则分摊到其他商品上
        // 满足当前促销的总优惠金额
        if (skuPromotionDetail == null || skuPromotionDetail.size() < 2) {
            return;
        }

        // clone skuPromotionDetail
        Map<String, Double> skuPromotionDetailClone = MapUtil.sortByValue(skuPromotionDetail, false);

        // 未满足优惠金额的商品
        long matchPromotionsZeroCount =
                skuVOList.stream().filter(l -> l.getPriceDetailDTO().getFlowPrice() == 0 && skuPromotionDetailClone.containsKey(l.getGoodsSku().getId())).count();
        // 参与当前促销活动的商品
        long matchPromotionsCount = skuVOList.stream().filter(l -> skuPromotionDetailClone.containsKey(l.getGoodsSku().getId())).count();
        if (matchPromotionsZeroCount == matchPromotionsCount) {
            return;
        }
        // 分配到其他商品的优惠金额
        AtomicReference<Double> balance = new AtomicReference<>(0D);
        StringBuilder lastSkuId = new StringBuilder();

        // 计数器
        int count = 0;


        // 检查是否有不满足优惠金额的商品
        filterEnoughSku(skuVOList, skuPromotionDetailClone, discountPrice, totalPrice, balance, lastSkuId, promotionTypeEnum, activityId);

        // 循环 优惠金额分摊，直到所有商品都满足优惠金额
        while (true) {
            // 如果还有剩余金额，则继续分摊
            if (balance.get() > 0) {
                skuPromotionDetailClone.remove(lastSkuId.toString());
                double lastDiscountPrice = CurrencyUtil.sub(discountPrice, skuPromotionDetail.get(lastSkuId.toString()));
                double lastTotalPrice = CurrencyUtil.sub(totalPrice, skuPromotionDetail.get(lastSkuId.toString()));
                filterEnoughSku(skuVOList, skuPromotionDetailClone, lastDiscountPrice, lastTotalPrice, balance, lastSkuId, promotionTypeEnum, activityId);
            } else {
                break;
            }
            count++;

            // 防止死循环
            if (count > skuPromotionDetail.size()) {
                break;
            }
        }
    }


    private static void filterEnoughSku(List<CartSkuVO> skuVOList, Map<String, Double> skuPromotionDetail,
                                                   Double discountPrice, Double totalPrice,
                                                   AtomicReference<Double> balance, StringBuilder lastSkuId,
                                                   PromotionTypeEnum promotionTypeEnum, String activityId) {
        AtomicInteger count = new AtomicInteger(skuPromotionDetail.size());
        AtomicReference<Double> countPrice = new AtomicReference<>(0D);
        for (String skuId : skuPromotionDetail.keySet()) {
            skuVOList.forEach(l -> {
                if (l.getGoodsSku().getId().equals(skuId)) {
                    count.getAndDecrement();

                    //商品金额占比
                    double point = CurrencyUtil.div(l.getPriceDetailDTO().getGoodsPrice(), totalPrice, 4);

                    //商品优惠金额
                    Double skuDiscountPrice;

                    if (count.get() > 0) {
                        //非最后一个商品，则按照比例计算
                        skuDiscountPrice = CurrencyUtil.mul(discountPrice, point);
                    } else {
                        // 如果是最后一个商品 则减去之前优惠的金额来进行计算
                        skuDiscountPrice = CurrencyUtil.sub(discountPrice, countPrice.get());


                    }

                    if (balance.get() > 0) {
                        // 分摊到其他商品： 其他商品原优惠金额 + 差额
                        calculateCartSkuPromotionsPrice(l, balance.get(), promotionTypeEnum, activityId);
                    }

                    // 如果商品优惠金额大于商品金额，则取商品金额。差额分摊到其他商品
                    if (skuDiscountPrice > l.getPriceDetailDTO().getGoodsPrice()) {
                        balance.set(CurrencyUtil.sub(skuDiscountPrice, l.getPriceDetailDTO().getGoodsPrice()));
                        lastSkuId.append(skuId);
                        skuDiscountPrice = l.getPriceDetailDTO().getGoodsPrice();
                    } else {
                        balance.set(0D);
                    }


                    countPrice.set(CurrencyUtil.add(countPrice.get(), skuDiscountPrice));

                }
            });
        }
    }

    /**
     * 检查活动有效时间
     *
     * @param startTime     活动开始时间
     * @param endTime       活动结束时间
     * @param promotionType 活动类型
     * @param promotionId   活动ID
     * @return 是否有效
     */
    private static boolean checkPromotionValidTime(Date startTime, Date endTime, String promotionType, String promotionId) {
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
