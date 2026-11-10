package cn.lili.modules.promotion.serviceimpl;

import cn.hutool.core.collection.CollUtil;
import com.alibaba.fastjson2.JSON;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.modules.promotion.entity.dos.*;
import cn.lili.modules.promotion.entity.dto.search.PromotionGoodsSearchParams;
import cn.lili.modules.promotion.entity.dto.search.SeckillSearchParams;
import cn.lili.modules.promotion.entity.enums.FullDiscountTypeEnum;
import cn.lili.modules.promotion.entity.enums.PromotionsStatusEnum;
import cn.lili.modules.promotion.service.*;
import cn.lili.modules.promotion.tools.PromotionTools;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * 促销业务层实现
 *
 * @author Chopper
 * @since 2020/8/21
 */
@Slf4j
@Service
public class PromotionServiceImpl implements PromotionService {

    @Autowired
    private SeckillService seckillService;

    @Autowired
    private SeckillApplyService seckillApplyService;

    @Autowired
    private FullDiscountService fullDiscountService;

    @Autowired
    private PintuanService pintuanService;

    @Autowired
    private CouponService couponService;

    @Autowired
    private PromotionGoodsService promotionGoodsService;

    @Autowired
    private PointsGoodsService pointsGoodsService;

    @Autowired
    private KanjiaActivityGoodsService kanjiaActivityGoodsService;

    /**
     * 获取当前进行的所有促销活动信息（满额活动按子类型拆分为 FULL_MINUS、FULL_RATE、FULL_GIFT 供装修组件选择，同时保留 FULL_DISCOUNT 兼容）
     *
     * @return 当前促销活动集合
     */
    @Override
    public Map<String, List<PromotionGoods>> getCurrentPromotion() {
        PromotionGoodsSearchParams searchParams = new PromotionGoodsSearchParams();
        searchParams.setPromotionStatus(PromotionsStatusEnum.START.name());
        List<PromotionGoods> promotionGoods = promotionGoodsService.listFindAll(searchParams);
        Map<String, List<PromotionGoods>> grouped = promotionGoods.stream()
                .filter(goods -> !PromotionTypeEnum.isRetired(goods.getPromotionType()))
                .collect(Collectors.groupingBy(PromotionGoods::getPromotionType));

        grouped.putIfAbsent(FullDiscountTypeEnum.FULL_MINUS.name(), new ArrayList<>());
        grouped.putIfAbsent(FullDiscountTypeEnum.FULL_RATE.name(), new ArrayList<>());
        grouped.putIfAbsent(FullDiscountTypeEnum.FULL_GIFT.name(), new ArrayList<>());

        List<PromotionGoods> fullDiscountGoods = grouped.get(PromotionTypeEnum.FULL_DISCOUNT.name());
        if (CollUtil.isNotEmpty(fullDiscountGoods)) {
            List<String> promotionIds = fullDiscountGoods.stream()
                    .map(PromotionGoods::getPromotionId)
                    .filter(Objects::nonNull)
                    .distinct()
                    .collect(Collectors.toList());
            Map<String, FullDiscount> fdMap = new HashMap<>();
            if (!promotionIds.isEmpty()) {
                List<FullDiscount> fullDiscounts = fullDiscountService.listByIds(promotionIds);
                if (fullDiscounts != null) {
                    for (FullDiscount fd : fullDiscounts) {
                        if (fd != null && fd.getId() != null) {
                            fdMap.put(fd.getId(), fd);
                        }
                    }
                }
            }
            for (PromotionGoods pg : fullDiscountGoods) {
                FullDiscount fd = fdMap.get(pg.getPromotionId());
                String subType = resolveSubType(fd);
                grouped.computeIfAbsent(subType, k -> new ArrayList<>()).add(pg);
            }
        }
        return grouped;
    }

    /**
     * 解析满额活动对应的子类型编码
     *
     * @param fd 满额活动实体
     * @return FULL_MINUS / FULL_RATE / FULL_GIFT
     */
    private String resolveSubType(FullDiscount fd) {
        if (fd == null) {
            return FullDiscountTypeEnum.FULL_MINUS.name();
        }
        if (FullDiscountTypeEnum.isValid(fd.getActivityType())) {
            return fd.getActivityType();
        }
        if (Boolean.TRUE.equals(fd.getFullRateFlag())) {
            return FullDiscountTypeEnum.FULL_RATE.name();
        }
        if (!Boolean.TRUE.equals(fd.getFullMinusFlag()) && Boolean.TRUE.equals(fd.getGiftFlag())) {
            return FullDiscountTypeEnum.FULL_GIFT.name();
        }
        return FullDiscountTypeEnum.FULL_MINUS.name();
    }

    /**
     * 根据商品索引获取当前商品索引的所有促销活动信息
     *
     * @param storeId    店铺id
     * @param goodsSkuId 商品skuId
     * @return 当前促销活动集合
     */
    @Override
    public Map<String, Object> getGoodsSkuPromotionMap(String storeId, String goodsSkuId) {
        String storeIds = storeId + "," + PromotionTools.PLATFORM_ID;
        List<PromotionGoods> promotionGoodsList = promotionGoodsService.findSkuValidPromotion(goodsSkuId, storeIds);
        return wrapperPromotionMapList(promotionGoodsList);
    }

    @Override
    public void removeByGoodsIds(String goodsIdsJsonStr) {
        List<String> goodsIds = JSON.parseArray(goodsIdsJsonStr, String.class);
        promotionGoodsService.deletePromotionGoodsByGoods(goodsIds);
        kanjiaActivityGoodsService.deleteByGoodsIds(goodsIds);
    }

    @Override
    public Map<String, Object> wrapperPromotionMapList(List<PromotionGoods> promotionGoodsList) {
        Map<String, Object> promotionMap = new HashMap<>();
        for (PromotionGoods promotionGoods : promotionGoodsList) {
            if (PromotionTypeEnum.isRetired(promotionGoods.getPromotionType())) {
                continue;
            }
            String esPromotionKey = promotionGoods.getPromotionType() + "-" + promotionGoods.getPromotionId();
            switch (PromotionTypeEnum.valueOf(promotionGoods.getPromotionType())) {
                case COUPON:
                    Coupon coupon = couponService.getById(promotionGoods.getPromotionId());
                    promotionMap.put(esPromotionKey, coupon);
                    break;
                case PINTUAN:
                    Pintuan pintuan = pintuanService.getById(promotionGoods.getPromotionId());
                    promotionMap.put(esPromotionKey, pintuan);
                    break;
                case FULL_DISCOUNT:
                    FullDiscount fullDiscount = fullDiscountService.getById(promotionGoods.getPromotionId());
                    promotionMap.put(esPromotionKey, fullDiscount);
                    break;
                case SECKILL:
                    this.getGoodsCurrentSeckill(esPromotionKey, promotionGoods, promotionMap);
                    break;
                case POINTS_GOODS:
                    PointsGoods pointsGoods = pointsGoodsService.getById(promotionGoods.getPromotionId());
                    promotionMap.put(esPromotionKey, pointsGoods);
                    break;
                default:
                    break;
            }
        }
        return promotionMap;
    }

    private void getGoodsCurrentSeckill(String esPromotionKey, PromotionGoods promotionGoods, Map<String, Object> promotionMap) {
        Seckill seckill = seckillService.getById(promotionGoods.getPromotionId());
        SeckillSearchParams searchParams = new SeckillSearchParams();
        searchParams.setSeckillId(promotionGoods.getPromotionId());
        searchParams.setSkuId(promotionGoods.getSkuId());
        List<SeckillApply> seckillApplyList = seckillApplyService.getSeckillApplyList(searchParams);
        if (seckillApplyList != null && !seckillApplyList.isEmpty()) {
            String[] split = seckill.getHours().split(",");
            int[] hoursSored = Arrays.stream(split).mapToInt(Integer::parseInt).toArray();
            Arrays.sort(hoursSored);
            seckill.setStartTime(promotionGoods.getStartTime());
            seckill.setEndTime(promotionGoods.getEndTime());
            promotionMap.put(esPromotionKey, seckill);
        }
    }

}
