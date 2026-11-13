package cn.lili.modules.promotion.support;

import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.support.GoodsSkuReadSupport;
import cn.lili.modules.promotion.entity.dos.*;
import cn.lili.modules.promotion.entity.dto.search.SeckillSearchParams;
import cn.lili.modules.promotion.entity.enums.PromotionsScopeTypeEnum;
import cn.lili.modules.promotion.entity.enums.PromotionsStatusEnum;
import cn.lili.modules.promotion.mapper.*;
import cn.lili.modules.promotion.tools.PromotionTools;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import org.springframework.stereotype.Component;
import java.util.*;

/**
 * SKU促销基础查询，集中提供绑定范围、活动元数据和有效活动价查询，不依赖促销或商品索引业务服务。
 * 索引、商品详情及促销服务复用相同查询规则，避免服务间循环依赖。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
@Component
public class SkuPromotionQuerySupport {
    private static final String SKU_ID_COLUMN = "sku_id";
    private final PromotionGoodsMapper promotionGoodsMapper;
    private final CouponMapper couponMapper;
    private final PintuanMapper pintuanMapper;
    private final FullDiscountMapper fullDiscountMapper;
    private final SeckillMapper seckillMapper;
    private final SeckillApplyMapper seckillApplyMapper;
    private final PointsGoodsMapper pointsGoodsMapper;
    private final GoodsSkuReadSupport skuReads;

    /**
     * 注入促销基础存储及SKU读取组件，不调用促销写入服务。
     *
     * @param promotionGoodsMapper 促销商品绑定存储
     * @param couponMapper 优惠券存储
     * @param pintuanMapper 拼团活动存储
     * @param fullDiscountMapper 满额活动存储
     * @param seckillMapper 秒杀活动存储
     * @param seckillApplyMapper 秒杀SKU申请存储
     * @param pointsGoodsMapper 积分商品活动存储
     * @param skuReads SKU基础读取组件
     */
    public SkuPromotionQuerySupport(PromotionGoodsMapper promotionGoodsMapper,
            CouponMapper couponMapper,
            PintuanMapper pintuanMapper,
            FullDiscountMapper fullDiscountMapper,
            SeckillMapper seckillMapper,
            SeckillApplyMapper seckillApplyMapper,
            PointsGoodsMapper pointsGoodsMapper,
            GoodsSkuReadSupport skuReads) {
        this.promotionGoodsMapper = promotionGoodsMapper;
        this.couponMapper = couponMapper;
        this.pintuanMapper = pintuanMapper;
        this.fullDiscountMapper = fullDiscountMapper;
        this.seckillMapper = seckillMapper;
        this.seckillApplyMapper = seckillApplyMapper;
        this.pointsGoodsMapper = pointsGoodsMapper;
        this.skuReads = skuReads;
    }

    /**
     * 按SKU、店铺及平台范围读取当前或待开始促销，沿用全店和分类范围规则。
     *
     * @param skuId SKU主键
     * @param storeIds 店铺与平台ID的英文逗号分隔串
     * @return 可关联的促销商品列表
     */
    public List<PromotionGoods> findSkuValidPromotion(String skuId, String storeIds) {

        GoodsSku sku = skuReads.getGoodsSkuByIdFromCache(skuId);
        if (sku == null) {
            return new ArrayList<>();
        }
        QueryWrapper<PromotionGoods> queryWrapper = new QueryWrapper<>();

        queryWrapper.and(i -> i.or(j -> j.eq(SKU_ID_COLUMN, skuId))
                .or(n -> n.eq("scope_type", PromotionsScopeTypeEnum.ALL.name()))
                .or(n -> n.and(k -> k.eq("scope_type", PromotionsScopeTypeEnum.PORTION_GOODS_CATEGORY.name())
                        .and(l -> l.like("scope_id", sku.getCategoryPath())))));
        queryWrapper.and(i -> i.or(PromotionTools.queryPromotionStatus(PromotionsStatusEnum.START)).or(PromotionTools.queryPromotionStatus(PromotionsStatusEnum.NEW)));
        queryWrapper.in("store_id", Arrays.asList(storeIds.split(",")));
        return promotionGoodsMapper.selectList(queryWrapper);
    }

    /**
     * 查询指定类型中当前有效的最低促销价，仅统计已开始且未过期的活动。
     *
     * @param skuId SKU主键
     * @param promotionTypes 活动类型编码列表
     * @return 最低促销价，单位元，无匹配时为null
     */
    public Double getValidPromotionsGoodsPrice(String skuId, List<String> promotionTypes) {
        QueryWrapper<PromotionGoods> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq(SKU_ID_COLUMN, skuId);
        queryWrapper.in("promotion_type", promotionTypes);
        queryWrapper.and(PromotionTools.queryPromotionStatus(PromotionsStatusEnum.START));
        return promotionGoodsMapper.selectPromotionsGoodsPrice(queryWrapper);
    }

    /**
     * 读取店铺及平台促销并组装索引元数据，不依赖促销写入、订单或索引服务。
     *
     * @param storeId 店铺主键
     * @param goodsSkuId SKU主键
     * @return 以活动类型和活动ID为键的促销映射
     */
    public Map<String, Object> getGoodsSkuPromotionMap(String storeId, String goodsSkuId) {
        String storeIds = storeId + "," + PromotionTools.PLATFORM_ID;
        List<PromotionGoods> promotionGoodsList = this.findSkuValidPromotion(goodsSkuId, storeIds);
        return wrapperPromotionMapList(promotionGoodsList);
    }

    /**
     * 按绑定记录读取促销元数据，跳过已下线的历史活动类型，保留原有映射键和覆盖规则。
     *
     * @param promotionGoodsList 已查询的促销商品记录
     * @return 活动元数据映射，不额外改变绑定查询的状态范围
     */
    public Map<String, Object> wrapperPromotionMapList(List<PromotionGoods> promotionGoodsList) {
        Map<String, Object> promotionMap = new HashMap<>();
        for (PromotionGoods promotionGoods : promotionGoodsList) {
            if (PromotionTypeEnum.isRetired(promotionGoods.getPromotionType())) {
                continue;
            }
            String esPromotionKey = promotionGoods.getPromotionType() + "-" + promotionGoods.getPromotionId();
            switch (PromotionTypeEnum.valueOf(promotionGoods.getPromotionType())) {
                case COUPON:
                    Coupon coupon = couponMapper.selectById(promotionGoods.getPromotionId());
                    promotionMap.put(esPromotionKey, coupon);
                    break;
                case PINTUAN:
                    Pintuan pintuan = pintuanMapper.selectById(promotionGoods.getPromotionId());
                    promotionMap.put(esPromotionKey, pintuan);
                    break;
                case FULL_DISCOUNT:
                    FullDiscount fullDiscount = fullDiscountMapper.selectById(promotionGoods.getPromotionId());
                    promotionMap.put(esPromotionKey, fullDiscount);
                    break;
                case SECKILL:
                    this.getGoodsCurrentSeckill(esPromotionKey, promotionGoods, promotionMap);
                    break;
                case POINTS_GOODS:
                    PointsGoods pointsGoods = pointsGoodsMapper.selectById(promotionGoods.getPromotionId());
                    promotionMap.put(esPromotionKey, pointsGoods);
                    break;
                default:
                    break;
            }
        }
        return promotionMap;
    }

    /**
     * 秒杀活动存在对应SKU申请时，沿用活动绑定的起止时间构建展示元数据。
     *
     * @param esPromotionKey 类型与活动ID组成的映射键
     * @param promotionGoods 促销绑定记录
     * @param promotionMap 待填充的促销映射
     */
    private void getGoodsCurrentSeckill(String esPromotionKey, PromotionGoods promotionGoods, Map<String, Object> promotionMap) {
        Seckill seckill = seckillMapper.selectById(promotionGoods.getPromotionId());
        SeckillSearchParams searchParams = new SeckillSearchParams();
        searchParams.setSeckillId(promotionGoods.getPromotionId());
        searchParams.setSkuId(promotionGoods.getSkuId());
        List<SeckillApply> seckillApplyList = seckillApplyMapper.selectList(searchParams.queryWrapper());
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
