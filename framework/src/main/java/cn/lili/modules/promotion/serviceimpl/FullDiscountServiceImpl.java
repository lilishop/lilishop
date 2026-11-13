package cn.lili.modules.promotion.serviceimpl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.text.CharSequenceUtil;
import com.alibaba.fastjson2.JSON;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.support.GoodsSkuReadSupport;
import cn.lili.modules.order.cart.entity.vo.FullDiscountVO;
import cn.lili.modules.promotion.entity.dos.FullDiscount;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.dto.FullDiscountGiftItem;
import cn.lili.modules.promotion.entity.dto.FullDiscountRuleDTO;
import cn.lili.modules.promotion.entity.dto.search.PromotionGoodsSearchParams;
import cn.lili.modules.promotion.entity.enums.FullDiscountTypeEnum;
import cn.lili.modules.promotion.entity.enums.PromotionsScopeTypeEnum;
import cn.lili.modules.promotion.entity.enums.PromotionsStatusEnum;
import cn.lili.modules.promotion.mapper.FullDiscountMapper;
import cn.lili.modules.promotion.service.FullDiscountService;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import cn.lili.modules.promotion.support.CouponPromotionReferenceSupport;
import cn.lili.modules.promotion.tools.PromotionTools;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import static cn.lili.modules.promotion.tools.PromotionTools.queryPromotionStatus;

/**
 * 满优惠业务层实现（支持满减 FULL_MINUS、满折 FULL_RATE、满赠 FULL_GIFT 及 1~3 级阶梯）
 * 赠品读取与赠券存在性校验复用独立支持组件，不反向依赖商品或优惠券业务服务。
 * <p>
 * 关键业务规则：
 * <ul>
 *     <li>同一店铺重叠时间段内：全品类（ALL）满额活动与任何满额活动互斥；指定商品（PORTION_GOODS）活动之间只要参与 SKU 无交集即可共存。</li>
 *     <li>每个活动支持 1~3 个金额阶梯，门槛金额必须严格递增；满减金额须严格递增且小于对应门槛；满折折扣数值须严格递减（折扣力度递增）；满赠每阶梯 1~3 个不重复 SKU，单品 1~10 件。</li>
 * </ul>
 *
 * @author Chopper
 * @since 2020/8/21
 */
@Service
public class FullDiscountServiceImpl extends AbstractPromotionsServiceImpl<FullDiscountMapper, FullDiscount> implements FullDiscountService {

    /**
     * 最大阶梯数（最多 3 级阶梯）
     */
    private static final int MAX_TIER_SIZE = 3;

    /**
     * 每级阶梯最多可选赠品 SKU 数量（3 个）
     */
    private static final int MAX_GIFT_PER_TIER = 3;

    /**
     * 单个赠品最大赠送件数（10 件）
     */
    private static final int MAX_GIFT_ITEM_NUM = 10;

    /**
     * 门槛金额上限
     */
    private static final double MAX_THRESHOLD_MONEY = 99999999.00D;

    @Autowired
    private CouponPromotionReferenceSupport couponReferences;
    @Autowired
    private PromotionGoodsService promotionGoodsService;
    @Autowired
    private GoodsSkuReadSupport skuReads;

    @Override
    public List<FullDiscountVO> currentPromotion(List<String> storeId) {
        List<FullDiscountVO> result = new ArrayList<>();
        QueryWrapper<FullDiscount> queryWrapper = new QueryWrapper<>();
        queryWrapper.in(storeId != null && !storeId.isEmpty(), "store_id", storeId);
        queryWrapper.and(PromotionTools.queryPromotionStatus(PromotionsStatusEnum.START));
        List<FullDiscount> list = this.list(queryWrapper);
        if (list != null) {
            for (FullDiscount fullDiscount : list) {
                PromotionGoodsSearchParams searchParams = new PromotionGoodsSearchParams();
                searchParams.setPromotionId(fullDiscount.getId());
                FullDiscountVO fullDiscountVO = new FullDiscountVO(fullDiscount);
                fullDiscountVO.setPromotionGoodsList(promotionGoodsService.listFindAll(searchParams));
                enrichGiftItemMetadata(fullDiscountVO);
                result.add(fullDiscountVO);
            }
        }
        return result;
    }

    /**
     * 获取满优惠活动详情（含阶梯规则及赠品 SKU 最新元数据）
     *
     * @param id 满优惠活动 ID
     * @return 满优惠活动详情 VO
     */
    @Override
    public FullDiscountVO getFullDiscount(String id) {
        FullDiscount fullDiscount = this.checkFullDiscountExist(id);
        FullDiscountVO fullDiscountVO = new FullDiscountVO(fullDiscount);
        PromotionGoodsSearchParams searchParams = new PromotionGoodsSearchParams();
        searchParams.setPromotionId(fullDiscount.getId());
        fullDiscountVO.setPromotionGoodsList(promotionGoodsService.listFindAll(searchParams));
        enrichGiftItemMetadata(fullDiscountVO);
        return fullDiscountVO;
    }

    /**
     * 允许在同店铺同时间段存在不同商品范围的满额活动，由 {@link #checkSameScopeConflict(FullDiscount)} 执行精细化冲突校验
     *
     * @return true
     */
    @Override
    public boolean allowExistSame() {
        return true;
    }

    /**
     * 检查促销参数（含活动时间、同店铺重叠时段全店/指定 SKU 互斥、1~3 级阶梯参数校验）
     *
     * @param promotions 促销实体
     */
    @Override
    public void checkPromotions(FullDiscount promotions) {
        super.checkPromotions(promotions);
        if (promotions instanceof FullDiscountVO) {
            FullDiscountVO fullDiscountVO = (FullDiscountVO) promotions;
            PromotionTools.checkPromotionTime(fullDiscountVO.getStartTime(), fullDiscountVO.getEndTime());
        }
        // 校验同店铺同时间段的全品类或指定 SKU 重叠冲突
        this.checkSameScopeConflict(promotions);
        // 检查满减/满折/满赠阶梯参数并同步冗余字段
        this.checkFullDiscount(promotions);
    }

    /**
     * 更新促销商品信息
     *
     * @param promotions 促销实体
     * @return 是否更新成功
     */
    @Override
    @Transactional(rollbackFor = {Exception.class})
    public boolean updatePromotionsGoods(FullDiscount promotions) {
        boolean result = super.updatePromotionsGoods(promotions);
        if (!PromotionsStatusEnum.CLOSE.name().equals(promotions.getPromotionStatus())
                && PromotionsScopeTypeEnum.PORTION_GOODS.name().equals(promotions.getScopeType())
                && promotions instanceof FullDiscountVO) {
            FullDiscountVO fullDiscountVO = (FullDiscountVO) promotions;
            List<PromotionGoods> promotionGoodsList =
                    PromotionTools.promotionGoodsInit(fullDiscountVO.getPromotionGoodsList(), fullDiscountVO,
                            PromotionTypeEnum.FULL_DISCOUNT);
            this.promotionGoodsService.deletePromotionGoods(Collections.singletonList(promotions.getId()));
            // 促销活动商品更新
            result = this.promotionGoodsService.saveBatch(promotionGoodsList);
        }
        return result;
    }

    /**
     * 更新促销信息到商品索引
     *
     * @param promotions 促销实体
     */
    @Override
    public void updateGoodsIndex(FullDiscount promotions) {
        super.updateGoodsIndex(promotions);
    }

    /**
     * 当前促销类型
     *
     * @return 当前促销类型
     */
    @Override
    public PromotionTypeEnum getPromotionType() {
        return PromotionTypeEnum.FULL_DISCOUNT;
    }

    /**
     * 校验同一店铺在重叠时间段内的活动范围与 SKU 冲突：
     * <ul>
     *     <li>若新活动为全品类（ALL），则重叠时间段内同店铺不能有任何满额类活动（全品类或指定商品）；</li>
     *     <li>若新活动为指定商品（PORTION_GOODS），则重叠时间段内同店铺不能有全品类活动，且不能与其他指定商品活动的 SKU 产生交集。</li>
     * </ul>
     *
     * @param promotions 待保存或更新的满额活动
     */
    protected void checkSameScopeConflict(FullDiscount promotions) {
        if (promotions.getStartTime() == null || promotions.getEndTime() == null
                || CharSequenceUtil.isBlank(promotions.getStoreId())) {
            return;
        }
        QueryWrapper<FullDiscount> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("store_id", promotions.getStoreId());
        queryWrapper.eq("delete_flag", false);
        if (CharSequenceUtil.isNotBlank(promotions.getId())) {
            queryWrapper.ne("id", promotions.getId());
        }
        queryWrapper.and(i -> i.or(queryPromotionStatus(PromotionsStatusEnum.NEW))
                .or(queryPromotionStatus(PromotionsStatusEnum.START)));
        queryWrapper.and(i -> i.or(j -> j.between("start_time",
                        DateUtil.beginOfDay(promotions.getStartTime()),
                        DateUtil.endOfDay(promotions.getEndTime()))
                .or().between("end_time",
                        DateUtil.beginOfDay(promotions.getStartTime()),
                        DateUtil.endOfDay(promotions.getEndTime()))
                .or(k -> k.le("start_time", promotions.getStartTime()).ge("end_time", promotions.getEndTime()))));

        List<FullDiscount> candidateList = this.list(queryWrapper);
        if (CollUtil.isEmpty(candidateList)) {
            return;
        }

        Set<String> currentSkuIds = extractScopeSkuIds(promotions);
        boolean currentIsAll = PromotionsScopeTypeEnum.ALL.name().equals(promotions.getScopeType());

        for (FullDiscount existing : candidateList) {
            if (existing.getStartTime() == null || existing.getEndTime() == null) {
                continue;
            }
            if (!isOverlapping(promotions.getStartTime(), promotions.getEndTime(),
                    existing.getStartTime(), existing.getEndTime())) {
                continue;
            }
            boolean existingIsAll = PromotionsScopeTypeEnum.ALL.name().equals(existing.getScopeType());
            // 任一方为全店活动，则必定冲突
            if (currentIsAll || existingIsAll) {
                throw new ServiceException(ResultCode.PROMOTION_SAME_ACTIVE_EXIST);
            }
            // 双方均为指定商品，检查 SKU 集合是否有交集
            Set<String> existingSkuIds = extractScopeSkuIds(existing);
            if (!Collections.disjoint(currentSkuIds, existingSkuIds)) {
                throw new ServiceException(ResultCode.PROMOTION_SKU_CONFLICT);
            }
        }
    }

    /**
     * 提取满额活动关联的指定商品 SKU ID 集合
     *
     * @param promotions 满额活动实体或 VO
     * @return SKU ID 集合
     */
    private Set<String> extractScopeSkuIds(FullDiscount promotions) {
        Set<String> skuIds = new HashSet<>();
        if (CharSequenceUtil.isNotBlank(promotions.getScopeId())) {
            Arrays.stream(promotions.getScopeId().split(","))
                    .map(String::trim)
                    .filter(CharSequenceUtil::isNotBlank)
                    .forEach(skuIds::add);
        }
        if (promotions instanceof FullDiscountVO) {
            FullDiscountVO vo = (FullDiscountVO) promotions;
            if (CollUtil.isNotEmpty(vo.getPromotionGoodsList())) {
                vo.getPromotionGoodsList().stream()
                        .filter(Objects::nonNull)
                        .map(PromotionGoods::getSkuId)
                        .filter(CharSequenceUtil::isNotBlank)
                        .forEach(skuIds::add);
            }
        }
        return skuIds;
    }

    /**
     * 检查满优惠活动是否存在
     *
     * @param id 满优惠活动 ID
     * @return 满优惠活动
     */
    private FullDiscount checkFullDiscountExist(String id) {
        FullDiscount fullDiscount = this.getById(id);
        if (fullDiscount == null) {
            throw new ServiceException(ResultCode.FULL_DISCOUNT_NOT_EXIST_ERROR);
        }
        return fullDiscount;
    }

    /**
     * 检查满减/满折/满赠参数（兼容阶梯列表与旧版单门槛入参），并同步冗余字段与 JSON
     *
     * @param fullDiscount 满优惠活动参数信息
     */
    private void checkFullDiscount(FullDiscount fullDiscount) {
        List<FullDiscountRuleDTO> ruleList = extractRuleList(fullDiscount);
        String activityType = resolveActivityType(fullDiscount, ruleList);
        if (!FullDiscountTypeEnum.isValid(activityType)) {
            throw new ServiceException(ResultCode.FULL_DISCOUNT_WAY_ERROR);
        }
        fullDiscount.setActivityType(activityType);

        if (CollUtil.isEmpty(ruleList) || ruleList.size() > MAX_TIER_SIZE) {
            throw new ServiceException(ResultCode.FULL_DISCOUNT_TIER_SIZE_ERROR);
        }

        double prevMoney = 0D;
        double prevMinus = 0D;
        double prevRate = 10D;

        for (int i = 0; i < ruleList.size(); i++) {
            FullDiscountRuleDTO rule = ruleList.get(i);
            if (rule == null || rule.getFullMoney() == null || rule.getFullMoney() <= 0
                    || rule.getFullMoney() > MAX_THRESHOLD_MONEY) {
                throw new ServiceException(ResultCode.FULL_DISCOUNT_TIER_THRESHOLD_ERROR);
            }
            if (i > 0 && rule.getFullMoney() <= prevMoney) {
                throw new ServiceException(ResultCode.FULL_DISCOUNT_TIER_THRESHOLD_ERROR);
            }
            prevMoney = rule.getFullMoney();

            if (FullDiscountTypeEnum.FULL_MINUS.name().equals(activityType)) {
                this.checkFullMinus(rule.getFullMinus(), rule.getFullMoney());
                if (i > 0 && rule.getFullMinus() <= prevMinus) {
                    throw new ServiceException(ResultCode.FULL_DISCOUNT_TIER_MINUS_ERROR);
                }
                prevMinus = rule.getFullMinus();
                rule.setFullRate(null);
                rule.setGiftItems(null);
            } else if (FullDiscountTypeEnum.FULL_RATE.name().equals(activityType)) {
                this.checkFullRate(rule.getFullRate());
                if (i > 0 && rule.getFullRate() >= prevRate) {
                    throw new ServiceException(ResultCode.FULL_DISCOUNT_TIER_RATE_ERROR);
                }
                prevRate = rule.getFullRate();
                rule.setFullMinus(null);
                rule.setGiftItems(null);
            } else if (FullDiscountTypeEnum.FULL_GIFT.name().equals(activityType)) {
                this.checkTierGiftItems(rule);
                rule.setFullMinus(null);
                rule.setFullRate(null);
            }
        }

        // 若历史单门槛入参带有优惠券，仍保持兼容校验
        if (Boolean.TRUE.equals(fullDiscount.getCouponFlag())) {
            this.checkCoupon(fullDiscount.getCouponId());
        } else {
            fullDiscount.setCouponFlag(false);
            fullDiscount.setCouponId(null);
        }

        // 将第 1 级阶梯同步至主表冗余字段，保持搜索、列表及旧版调用方兼容
        FullDiscountRuleDTO firstRule = ruleList.get(0);
        fullDiscount.setFullMoney(firstRule.getFullMoney());
        if (FullDiscountTypeEnum.FULL_MINUS.name().equals(activityType)) {
            fullDiscount.setFullMinusFlag(true);
            fullDiscount.setFullMinus(firstRule.getFullMinus());
            fullDiscount.setFullRateFlag(false);
            fullDiscount.setFullRate(null);
            fullDiscount.setGiftFlag(false);
            fullDiscount.setGiftId(null);
        } else if (FullDiscountTypeEnum.FULL_RATE.name().equals(activityType)) {
            fullDiscount.setFullMinusFlag(false);
            fullDiscount.setFullMinus(null);
            fullDiscount.setFullRateFlag(true);
            fullDiscount.setFullRate(firstRule.getFullRate());
            fullDiscount.setGiftFlag(false);
            fullDiscount.setGiftId(null);
        } else if (FullDiscountTypeEnum.FULL_GIFT.name().equals(activityType)) {
            fullDiscount.setFullMinusFlag(false);
            fullDiscount.setFullMinus(null);
            fullDiscount.setFullRateFlag(false);
            fullDiscount.setFullRate(null);
            fullDiscount.setGiftFlag(true);
            String firstTierGiftIds = firstRule.getGiftItems().stream()
                    .map(FullDiscountGiftItem::getSkuId)
                    .filter(CharSequenceUtil::isNotBlank)
                    .collect(Collectors.joining(","));
            fullDiscount.setGiftId(firstTierGiftIds);
        }

        fullDiscount.setRulesJson(JSON.toJSONString(ruleList));
        if (fullDiscount instanceof FullDiscountVO) {
            ((FullDiscountVO) fullDiscount).setRuleList(ruleList);
        }
        fullDiscount.setTitle(buildSummaryTitle(activityType, ruleList));
    }

    /**
     * 从入参提取阶梯规则列表（支持 VO.ruleList、rulesJson 及旧版单门槛字段自动转换）
     *
     * @param fullDiscount 满额活动实体或 VO
     * @return 阶梯规则列表
     */
    private List<FullDiscountRuleDTO> extractRuleList(FullDiscount fullDiscount) {
        if (fullDiscount instanceof FullDiscountVO) {
            FullDiscountVO vo = (FullDiscountVO) fullDiscount;
            if (CollUtil.isNotEmpty(vo.getRuleList())) {
                return vo.getRuleList();
            }
        }
        if (CharSequenceUtil.isNotBlank(fullDiscount.getRulesJson())) {
            try {
                List<FullDiscountRuleDTO> parsed = JSON.parseArray(fullDiscount.getRulesJson(), FullDiscountRuleDTO.class);
                if (CollUtil.isNotEmpty(parsed)) {
                    return parsed;
                }
            } catch (Exception ignored) {
                // 解析失败走下方单门槛兜底
            }
        }
        if (fullDiscount.getFullMoney() != null) {
            FullDiscountRuleDTO singleRule = new FullDiscountRuleDTO();
            singleRule.setFullMoney(fullDiscount.getFullMoney());
            singleRule.setFullMinus(fullDiscount.getFullMinus());
            singleRule.setFullRate(fullDiscount.getFullRate());
            if (Boolean.TRUE.equals(fullDiscount.getGiftFlag()) && CharSequenceUtil.isNotBlank(fullDiscount.getGiftId())) {
                List<FullDiscountGiftItem> giftItems = Arrays.stream(fullDiscount.getGiftId().split(","))
                        .map(String::trim)
                        .filter(CharSequenceUtil::isNotBlank)
                        .map(skuId -> FullDiscountGiftItem.builder().skuId(skuId).num(1).build())
                        .collect(Collectors.toList());
                singleRule.setGiftItems(giftItems);
            }
            return new ArrayList<>(Collections.singletonList(singleRule));
        }
        return Collections.emptyList();
    }

    /**
     * 解析活动子类型（优先取显式传入的 activityType，未传时根据标志位或阶梯字段推断）
     *
     * @param fullDiscount 满额活动
     * @param ruleList     阶梯列表
     * @return 活动子类型编码
     */
    private String resolveActivityType(FullDiscount fullDiscount, List<FullDiscountRuleDTO> ruleList) {
        if (FullDiscountTypeEnum.isValid(fullDiscount.getActivityType())) {
            return fullDiscount.getActivityType();
        }
        if (Boolean.TRUE.equals(fullDiscount.getFullMinusFlag())) {
            return FullDiscountTypeEnum.FULL_MINUS.name();
        }
        if (Boolean.TRUE.equals(fullDiscount.getFullRateFlag())) {
            return FullDiscountTypeEnum.FULL_RATE.name();
        }
        if (Boolean.TRUE.equals(fullDiscount.getGiftFlag())) {
            return FullDiscountTypeEnum.FULL_GIFT.name();
        }
        if (CollUtil.isNotEmpty(ruleList)) {
            FullDiscountRuleDTO first = ruleList.get(0);
            if (first != null) {
                if (first.getFullMinus() != null && first.getFullMinus() > 0) {
                    return FullDiscountTypeEnum.FULL_MINUS.name();
                }
                if (first.getFullRate() != null && first.getFullRate() > 0) {
                    return FullDiscountTypeEnum.FULL_RATE.name();
                }
                if (CollUtil.isNotEmpty(first.getGiftItems())) {
                    return FullDiscountTypeEnum.FULL_GIFT.name();
                }
            }
        }
        return null;
    }

    /**
     * 校验满赠单级阶梯的赠品列表（1~3 个不重复赠品 SKU，每个赠品 1~10 件，并回填商品信息快照）
     *
     * @param rule 单级阶梯规则
     */
    private void checkTierGiftItems(FullDiscountRuleDTO rule) {
        List<FullDiscountGiftItem> giftItems = rule.getGiftItems();
        if (CollUtil.isEmpty(giftItems)) {
            throw new ServiceException(ResultCode.FULL_DISCOUNT_GIFT_ERROR);
        }
        if (giftItems.size() > MAX_GIFT_PER_TIER) {
            throw new ServiceException(ResultCode.FULL_DISCOUNT_GIFT_SIZE_ERROR);
        }
        Set<String> seenSkuIds = new HashSet<>();
        for (FullDiscountGiftItem item : giftItems) {
            if (item == null || CharSequenceUtil.isBlank(item.getSkuId())) {
                throw new ServiceException(ResultCode.FULL_DISCOUNT_GIFT_ERROR);
            }
            if (!seenSkuIds.add(item.getSkuId())) {
                throw new ServiceException(ResultCode.FULL_DISCOUNT_GIFT_SIZE_ERROR);
            }
            int num = item.getNum() == null ? 1 : item.getNum();
            if (num < 1 || num > MAX_GIFT_ITEM_NUM) {
                throw new ServiceException(ResultCode.FULL_DISCOUNT_GIFT_NUM_ERROR);
            }
            item.setNum(num);
            GoodsSku giftSku = this.checkGiftSku(item.getSkuId());
            item.setGoodsId(giftSku.getGoodsId());
            item.setGoodsName(giftSku.getGoodsName());
            item.setThumbnail(giftSku.getThumbnail());
            item.setSimpleSpecs(giftSku.getSimpleSpecs());
            item.setGoodsType(giftSku.getGoodsType());
            item.setPrice(giftSku.getPrice());
        }
    }

    /**
     * 为详情 VO 补齐阶梯赠品的最新商品名称与缩略图
     *
     * @param fullDiscountVO 满额活动 VO
     */
    private void enrichGiftItemMetadata(FullDiscountVO fullDiscountVO) {
        if (fullDiscountVO == null || CollUtil.isEmpty(fullDiscountVO.getRuleList())) {
            return;
        }
        for (FullDiscountRuleDTO rule : fullDiscountVO.getRuleList()) {
            if (rule == null || CollUtil.isEmpty(rule.getGiftItems())) {
                continue;
            }
            for (FullDiscountGiftItem item : rule.getGiftItems()) {
                if (item != null && CharSequenceUtil.isNotBlank(item.getSkuId())
                        && CharSequenceUtil.isBlank(item.getGoodsName()) && this.skuReads != null) {
                    GoodsSku sku = this.skuReads.getGoodsSkuByIdFromCache(item.getSkuId());
                    if (sku != null) {
                        item.setGoodsId(sku.getGoodsId());
                        item.setGoodsName(sku.getGoodsName());
                        item.setThumbnail(sku.getThumbnail());
                        item.setSimpleSpecs(sku.getSimpleSpecs());
                        item.setGoodsType(sku.getGoodsType());
                        item.setPrice(sku.getPrice());
                    }
                }
            }
        }
        if (Boolean.TRUE.equals(fullDiscountVO.getGiftFlag()) && CharSequenceUtil.isNotBlank(fullDiscountVO.getGiftId())
                && CharSequenceUtil.isBlank(fullDiscountVO.getGiftSkuName()) && this.skuReads != null) {
            String firstSkuId = fullDiscountVO.getGiftId().split(",")[0].trim();
            GoodsSku sku = this.skuReads.getGoodsSkuByIdFromCache(firstSkuId);
            if (sku != null) {
                fullDiscountVO.setGiftSkuId(firstSkuId);
                fullDiscountVO.setGiftSkuName(sku.getGoodsName());
                fullDiscountVO.setGiftSkuThumbnail(sku.getThumbnail());
            }
        }
    }

    /**
     * 构建活动阶梯汇总标题
     *
     * @param activityType 活动子类型
     * @param ruleList     阶梯规则列表
     * @return 汇总标题字符串
     */
    private String buildSummaryTitle(String activityType, List<FullDiscountRuleDTO> ruleList) {
        List<String> parts = new ArrayList<>();
        for (FullDiscountRuleDTO rule : ruleList) {
            if (FullDiscountTypeEnum.FULL_MINUS.name().equals(activityType)) {
                parts.add("满" + rule.getFullMoney() + " 减" + rule.getFullMinus());
            } else if (FullDiscountTypeEnum.FULL_RATE.name().equals(activityType)) {
                parts.add("满" + rule.getFullMoney() + " 打" + rule.getFullRate() + "折");
            } else if (FullDiscountTypeEnum.FULL_GIFT.name().equals(activityType)) {
                String giftNames = CollUtil.isEmpty(rule.getGiftItems()) ? "赠礼" :
                        rule.getGiftItems().stream()
                                .map(g -> (CharSequenceUtil.isNotBlank(g.getGoodsName()) ? g.getGoodsName() : "赠品")
                                        + "×" + g.validNum())
                                .collect(Collectors.joining("、"));
                parts.add("满" + rule.getFullMoney() + " 赠" + giftNames);
            }
        }
        return String.join("；", parts);
    }

    /**
     * 检查赠品 SKU（E_COUPON 可作为满赠赠品，支付后由卡池自动发卡）
     *
     * @param giftSkuId 赠品 SKU 编号
     * @return 赠品 SKU 实体
     */
    private GoodsSku checkGiftSku(String giftSkuId) {
        GoodsSku giftSku = this.skuReads.getGoodsSkuByIdFromCache(giftSkuId);
        if (giftSku == null) {
            throw new ServiceException(ResultCode.FULL_DISCOUNT_GIFT_ERROR);
        }
        return giftSku;
    }

    /**
     * 通过公共关联校验检查赠送优惠券是否存在，不额外改变状态、有效期或店铺限制。
     *
     * @param couponId 优惠券编号
     */
    private void checkCoupon(String couponId) {
        couponReferences.checkCouponExists(couponId);
    }

    /**
     * 检查满减信息
     *
     * @param fullMinus 满减金额
     * @param fullMoney 优惠门槛
     */
    private void checkFullMinus(Double fullMinus, Double fullMoney) {
        boolean noFullMinusInput = fullMinus == null || fullMinus <= 0;
        if (noFullMinusInput) {
            throw new ServiceException(ResultCode.FULL_DISCOUNT_MONEY_ERROR);
        }
        if (fullMinus >= fullMoney) {
            throw new ServiceException(ResultCode.FULL_DISCOUNT_MONEY_GREATER_THAN_MINUS);
        }
    }

    /**
     * 检查打折信息
     *
     * @param fullRate 打折数值
     */
    private void checkFullRate(Double fullRate) {
        boolean noFullRateInput = fullRate == null || fullRate <= 0;
        if (noFullRateInput) {
            throw new ServiceException(ResultCode.FULL_RATE_NUM_ERROR);
        }
        int rateLimit = 10;
        if (fullRate >= rateLimit || fullRate <= 0) {
            throw new ServiceException(ResultCode.FULL_RATE_NUM_ERROR);
        }
    }
}
