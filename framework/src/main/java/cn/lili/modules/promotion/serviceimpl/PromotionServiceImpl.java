package cn.lili.modules.promotion.serviceimpl;

import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.modules.promotion.entity.dos.*;
import cn.lili.modules.promotion.entity.dto.search.PromotionGoodsSearchParams;
import cn.lili.modules.promotion.entity.dto.search.SeckillSearchParams;
import cn.lili.modules.promotion.entity.enums.PromotionsStatusEnum;
import cn.lili.modules.promotion.service.*;
import cn.lili.modules.promotion.tools.PromotionTools;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
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
    /**
     * 秒杀
     */
    @Autowired
    private SeckillService seckillService;
    /**
     * 秒杀申请
     */
    @Autowired
    private SeckillApplyService seckillApplyService;
    /**
     * 满额活动
     */
    @Autowired
    private FullDiscountService fullDiscountService;
    /**
     * 拼团
     */
    @Autowired
    private PintuanService pintuanService;
    /**
     * 优惠券
     */
    @Autowired
    private CouponService couponService;
    /**
     * 促销商品
     */
    @Autowired
    private PromotionGoodsService promotionGoodsService;
    /**
     * 积分商品
     */
    @Autowired
    private PointsGoodsService pointsGoodsService;


    /**
     * 获取当前进行的所有促销活动信息
     *
     * @return 当前促销活动集合
     */
    @Override
    public Map<String, List<PromotionGoods>> getCurrentPromotion() {
        PromotionGoodsSearchParams searchParams = new PromotionGoodsSearchParams();
        searchParams.setPromotionStatus(PromotionsStatusEnum.START.name());
        List<PromotionGoods> promotionGoods = promotionGoodsService.listFindAll(searchParams);
        return promotionGoods.stream().collect(Collectors.groupingBy(PromotionGoods::getPromotionType));
    }

    /**
     * 根据商品索引获取当前商品索引的所有促销活动信息
     *
     * @param storeId    店铺id
     * @param goodsSkuId 商品skuId
     * @return 当前促销活动集合
     */
    public Map<String, Object> getGoodsSkuPromotionMap(String storeId, String goodsSkuId) {
        String storeIds = storeId + "," + PromotionTools.PLATFORM_ID;
        Map<String, Object> promotionMap = new HashMap<>();
        List<PromotionGoods> promotionGoodsList = promotionGoodsService.findSkuValidPromotion(goodsSkuId, storeIds);
        for (PromotionGoods promotionGoods : promotionGoodsList) {
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