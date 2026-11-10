package cn.lili.modules.order.cart.render.impl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.text.CharSequenceUtil;
import com.alibaba.fastjson2.JSON;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.RenderStepEnums;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.cart.entity.vo.FullDiscountVO;
import cn.lili.modules.order.cart.render.CartRenderStep;
import cn.lili.modules.order.cart.render.util.PromotionPriceUtil;
import cn.lili.modules.order.order.entity.dto.DiscountPriceItem;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.promotion.entity.dos.FullDiscount;
import cn.lili.modules.promotion.entity.dto.FullDiscountGiftItem;
import cn.lili.modules.promotion.entity.dto.FullDiscountRuleDTO;
import cn.lili.modules.promotion.entity.enums.FullDiscountTypeEnum;
import cn.lili.modules.promotion.entity.enums.PromotionsScopeTypeEnum;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * 满优惠（满减 / 满折 / 满赠）多活动分组与三级阶梯渲染器
 * <p>
 * 关键业务规则：
 * <ul>
 *     <li>同一店铺购物车内支持同时存在多个满额活动（针对不同指定 SKU 的满减、满折、满赠），按活动 ID 独立汇总各组已选 SKU 的商品金额并同时生效。</li>
 *     <li>每个活动支持 1~3 个金额门槛阶梯，按门槛从高到低匹配最高满足的一档阶梯执行立减、打折或赠送商品。</li>
 *     <li>生成递进式阶梯凑单提示文案（未达首档提示差额、已达某档提示再买差额享下一档、已达最高档提示已享最高优惠）。</li>
 * </ul>
 *
 * @author Chopper
 * @since 2020-04-01 10:27 上午
 */
@Service
public class FullDiscountRender implements CartRenderStep {

    @Autowired
    private GoodsSkuService goodsSkuService;

    @Override
    public RenderStepEnums step() {
        return RenderStepEnums.FULL_DISCOUNT;
    }

    @Override
    public void render(TradeDTO tradeDTO) {
        List<CartVO> cartList = tradeDTO.getCartList();
        if (CollUtil.isEmpty(cartList)) {
            return;
        }

        for (CartVO cart : cartList) {
            if (CollUtil.isEmpty(cart.getSkuList())) {
                continue;
            }
            // 提取当前店铺购物车内所有 SKU 关联的满额活动（按活动 ID 保序去重）
            Map<String, FullDiscount> activityMap = extractDistinctFullDiscounts(cart.getSkuList());
            if (activityMap.isEmpty()) {
                continue;
            }

            List<FullDiscountVO> activeVoList = new ArrayList<>();
            Set<String> allFullDiscountSkuIds = new LinkedHashSet<>();
            List<String> noticeParts = new ArrayList<>();
            Map<String, FullDiscountGiftItem> mergedGifts = new LinkedHashMap<>();
            Set<String> legacyGiftSkuIds = new LinkedHashSet<>();
            boolean anyFull = false;

            for (FullDiscount fullDiscount : activityMap.values()) {
                FullDiscountVO fullDiscountVO = buildFullDiscountVO(fullDiscount);
                activeVoList.add(fullDiscountVO);

                // 统计该活动适用范围内已勾选 SKU 的计价基数（商品原小计 - 付费会员补贴）
                Map<String, Double> skuPriceDetail = collectEligibleCheckedSkuPrices(cart, fullDiscountVO);
                if (skuPriceDetail.isEmpty()) {
                    continue;
                }
                allFullDiscountSkuIds.addAll(skuPriceDetail.keySet());
                Double countPrice = countPrice(skuPriceDetail);

                List<FullDiscountRuleDTO> ruleList = fullDiscountVO.resolveRuleList();
                int matchedIndex = matchHighestTierIndex(countPrice, ruleList);
                if (matchedIndex >= 0) {
                    anyFull = true;
                    FullDiscountRuleDTO matchedRule = ruleList.get(matchedIndex);
                    String activityType = fullDiscountVO.resolveCompatibleActivityType();

                    if (FullDiscountTypeEnum.FULL_MINUS.name().equals(activityType)
                            && matchedRule.getFullMinus() != null && matchedRule.getFullMinus() > 0) {
                        PromotionPriceUtil.recountPrice(tradeDTO, skuPriceDetail, matchedRule.getFullMinus(),
                                PromotionTypeEnum.FULL_DISCOUNT, fullDiscountVO.getId());
                    } else if (FullDiscountTypeEnum.FULL_RATE.name().equals(activityType)
                            && matchedRule.getFullRate() != null && matchedRule.getFullRate() > 0) {
                        this.renderFullRate(cart, skuPriceDetail,
                                CurrencyUtil.div(matchedRule.getFullRate(), 10), fullDiscountVO.getId());
                    } else if (FullDiscountTypeEnum.FULL_GIFT.name().equals(activityType)) {
                        collectMatchedGifts(matchedRule, fullDiscountVO, mergedGifts, legacyGiftSkuIds);
                    }

                    // 兼容历史活动的附属赠券、赠积分、免运费、附属赠品
                    renderLegacyExtraRights(cart, fullDiscountVO, matchedRule, mergedGifts, legacyGiftSkuIds);
                }

                String notice = buildTierPromotionNotice(countPrice, fullDiscountVO, ruleList, matchedIndex);
                if (CharSequenceUtil.isNotBlank(notice)) {
                    noticeParts.add(notice);
                }
            }

            if (!activeVoList.isEmpty()) {
                cart.setFullDiscount(activeVoList.get(0));
                cart.setFullDiscountList(activeVoList);
            }
            if (!allFullDiscountSkuIds.isEmpty()) {
                cart.setFullDiscountSkuIds(new ArrayList<>(allFullDiscountSkuIds));
            }
            cart.setIsFull(anyFull);
            if (!mergedGifts.isEmpty()) {
                cart.setGiftItemList(new ArrayList<>(mergedGifts.values()));
            }
            if (!legacyGiftSkuIds.isEmpty()) {
                cart.setGiftList(new ArrayList<>(legacyGiftSkuIds));
            }
            if (!noticeParts.isEmpty()) {
                cart.setPromotionNotice(String.join(" ｜ ", noticeParts));
            }
        }
    }

    /**
     * 从店铺购物车 SKU 列表中提取所有去重后的满额活动实体
     *
     * @param skuList 购物车 SKU 列表
     * @return 按活动 ID 映射的满额活动 Map
     */
    private Map<String, FullDiscount> extractDistinctFullDiscounts(List<CartSkuVO> skuList) {
        Map<String, FullDiscount> map = new LinkedHashMap<>();
        for (CartSkuVO skuVO : skuList) {
            if (skuVO == null || CollUtil.isEmpty(skuVO.getPromotionMap())) {
                continue;
            }
            for (Map.Entry<String, Object> entry : skuVO.getPromotionMap().entrySet()) {
                if (entry.getKey() == null || !entry.getKey().contains(PromotionTypeEnum.FULL_DISCOUNT.name())
                        || entry.getValue() == null) {
                    continue;
                }
                FullDiscount fd = parseFullDiscount(entry.getValue());
                if (fd != null) {
                    String key = CharSequenceUtil.isNotBlank(fd.getId()) ? fd.getId() : entry.getKey();
                    map.putIfAbsent(key, fd);
                }
            }
        }
        return map;
    }

    /**
     * 将缓存或 JSON 对象反序列化为 {@link FullDiscount}
     *
     * @param raw 原始促销对象
     * @return 满额活动实体
     */
    private FullDiscount parseFullDiscount(Object raw) {
        if (raw instanceof FullDiscount) {
            return (FullDiscount) raw;
        }
        if (raw instanceof String) {
            return JSON.parseObject((String) raw, FullDiscount.class);
        }
        return JSON.parseObject(JSON.toJSONString(raw), FullDiscount.class);
    }

    /**
     * 构建并补齐赠品名称的 {@link FullDiscountVO}
     *
     * @param fullDiscount 满额活动实体
     * @return 满额活动 VO
     */
    private FullDiscountVO buildFullDiscountVO(FullDiscount fullDiscount) {
        FullDiscountVO fullDiscountVO = new FullDiscountVO(fullDiscount);
        if (Boolean.TRUE.equals(fullDiscount.getGiftFlag()) && CharSequenceUtil.isNotBlank(fullDiscount.getGiftId())
                && goodsSkuService != null) {
            String firstGiftId = fullDiscount.getGiftId().split(",")[0].trim();
            GoodsSku goodsSku = goodsSkuService.getGoodsSkuByIdFromCache(firstGiftId);
            if (goodsSku != null) {
                fullDiscountVO.setGiftSkuId(firstGiftId);
                fullDiscountVO.setGiftSkuName(goodsSku.getGoodsName());
                fullDiscountVO.setGiftSkuThumbnail(goodsSku.getThumbnail());
            }
        }
        if (CollUtil.isNotEmpty(fullDiscountVO.getRuleList()) && goodsSkuService != null) {
            for (FullDiscountRuleDTO rule : fullDiscountVO.getRuleList()) {
                if (rule == null || CollUtil.isEmpty(rule.getGiftItems())) {
                    continue;
                }
                for (FullDiscountGiftItem item : rule.getGiftItems()) {
                    if (item != null && CharSequenceUtil.isNotBlank(item.getSkuId())
                            && CharSequenceUtil.isBlank(item.getGoodsName())) {
                        GoodsSku sku = goodsSkuService.getGoodsSkuByIdFromCache(item.getSkuId());
                        if (sku != null) {
                            item.setGoodsId(sku.getGoodsId());
                            item.setGoodsName(sku.getGoodsName());
                            item.setThumbnail(sku.getThumbnail());
                            item.setGoodsType(sku.getGoodsType());
                            item.setPrice(sku.getPrice());
                        }
                    }
                }
            }
        }
        return fullDiscountVO;
    }

    /**
     * 筛选当前活动适用范围内且已勾选的购物车 SKU 价格明细
     *
     * @param cart           店铺购物车
     * @param fullDiscountVO 满额活动 VO
     * @return SKU ID 到参与满额计算金额的映射
     */
    private Map<String, Double> collectEligibleCheckedSkuPrices(CartVO cart, FullDiscountVO fullDiscountVO) {
        Map<String, Double> skuPriceDetail = new LinkedHashMap<>(16);
        Set<String> scopeSkuSet = null;
        if (PromotionsScopeTypeEnum.PORTION_GOODS.name().equals(fullDiscountVO.getScopeType())
                && CharSequenceUtil.isNotBlank(fullDiscountVO.getScopeId())) {
            scopeSkuSet = Arrays.stream(fullDiscountVO.getScopeId().split(","))
                    .map(String::trim)
                    .filter(CharSequenceUtil::isNotBlank)
                    .collect(Collectors.toSet());
        }
        for (CartSkuVO cartSkuVO : cart.getSkuList()) {
            if (cartSkuVO == null || !Boolean.TRUE.equals(cartSkuVO.getChecked()) || cartSkuVO.getGoodsSku() == null) {
                continue;
            }
            String skuId = cartSkuVO.getGoodsSku().getId();
            if (scopeSkuSet != null && !scopeSkuSet.contains(skuId)) {
                continue;
            }
            // 若该 SKU 自身带有 promotionMap 且仅参与特定活动，校验是否包含当前活动（全品类活动直接放行）
            if (!PromotionsScopeTypeEnum.ALL.name().equals(fullDiscountVO.getScopeType())
                    && scopeSkuSet == null && CharSequenceUtil.isNotBlank(fullDiscountVO.getId())) {
                boolean boundToActivity = cartSkuVO.getPromotionMap() != null
                        && cartSkuVO.getPromotionMap().keySet().stream()
                        .anyMatch(k -> k.equals(PromotionTypeEnum.FULL_DISCOUNT.name() + "-" + fullDiscountVO.getId()));
                if (!boundToActivity) {
                    continue;
                }
            }
            double basePrice = CurrencyUtil.sub(
                    cartSkuVO.getPriceDetailDTO().getGoodsPrice(),
                    cartSkuVO.getPriceDetailDTO().getPaidMemberSubsidy());
            skuPriceDetail.put(skuId, basePrice);
        }
        return skuPriceDetail;
    }

    /**
     * 匹配满足当前商品合计金额的最高阶梯下标（未满足任何阶梯返回 -1）
     *
     * @param countPrice 活动商品合计金额
     * @param ruleList   按门槛升序排列的阶梯列表
     * @return 命中的最高阶梯下标，未命中返回 -1
     */
    private int matchHighestTierIndex(Double countPrice, List<FullDiscountRuleDTO> ruleList) {
        if (countPrice == null || CollUtil.isEmpty(ruleList)) {
            return -1;
        }
        int matched = -1;
        for (int i = 0; i < ruleList.size(); i++) {
            FullDiscountRuleDTO rule = ruleList.get(i);
            if (rule != null && rule.getFullMoney() != null && countPrice >= rule.getFullMoney()) {
                matched = i;
            }
        }
        return matched;
    }

    /**
     * 收集命中满赠阶梯的赠品项（同店铺多满赠活动命中相同赠品 SKU 时累加赠送数量）
     *
     * @param matchedRule      命中的阶梯规则
     * @param fullDiscountVO   满额活动 VO
     * @param mergedGifts      汇总的赠品项 Map
     * @param legacyGiftSkuIds 兼容旧版的赠品 SKU ID 集合
     */
    private void collectMatchedGifts(FullDiscountRuleDTO matchedRule,
                                     FullDiscountVO fullDiscountVO,
                                     Map<String, FullDiscountGiftItem> mergedGifts,
                                     Set<String> legacyGiftSkuIds) {
        if (matchedRule != null && CollUtil.isNotEmpty(matchedRule.getGiftItems())) {
            for (FullDiscountGiftItem item : matchedRule.getGiftItems()) {
                if (item == null || CharSequenceUtil.isBlank(item.getSkuId())) {
                    continue;
                }
                String skuId = item.getSkuId().trim();
                legacyGiftSkuIds.add(skuId);
                FullDiscountGiftItem existing = mergedGifts.get(skuId);
                if (existing == null) {
                    mergedGifts.put(skuId, FullDiscountGiftItem.builder()
                            .skuId(skuId)
                            .goodsId(item.getGoodsId())
                            .goodsName(CharSequenceUtil.isNotBlank(item.getGoodsName())
                                    ? item.getGoodsName() : fullDiscountVO.getGiftSkuName())
                            .thumbnail(CharSequenceUtil.isNotBlank(item.getThumbnail())
                                    ? item.getThumbnail() : fullDiscountVO.getGiftSkuThumbnail())
                            .simpleSpecs(item.getSimpleSpecs())
                            .goodsType(item.getGoodsType())
                            .price(item.getPrice())
                            .num(item.validNum())
                            .build());
                } else {
                    existing.setNum(existing.validNum() + item.validNum());
                }
            }
        } else if (Boolean.TRUE.equals(fullDiscountVO.getGiftFlag())
                && CharSequenceUtil.isNotBlank(fullDiscountVO.getGiftId())) {
            for (String rawSkuId : fullDiscountVO.getGiftId().split(",")) {
                String skuId = rawSkuId.trim();
                if (CharSequenceUtil.isBlank(skuId)) {
                    continue;
                }
                legacyGiftSkuIds.add(skuId);
                mergedGifts.putIfAbsent(skuId, FullDiscountGiftItem.builder()
                        .skuId(skuId)
                        .goodsName(fullDiscountVO.getGiftSkuName())
                        .thumbnail(fullDiscountVO.getGiftSkuThumbnail())
                        .num(1)
                        .build());
            }
        }
    }

    /**
     * 兼容渲染历史旧版满优惠活动中的附属权益（送券、送积分、免邮、满减附带赠品）
     */
    private void renderLegacyExtraRights(CartVO cartVO,
                                         FullDiscountVO fullDiscount,
                                         FullDiscountRuleDTO matchedRule,
                                         Map<String, FullDiscountGiftItem> mergedGifts,
                                         Set<String> legacyGiftSkuIds) {
        if (Boolean.TRUE.equals(fullDiscount.getCouponFlag())
                && CharSequenceUtil.isNotBlank(fullDiscount.getCouponId())) {
            cartVO.getGiftCouponList().add(fullDiscount.getCouponId());
        }
        if (!FullDiscountTypeEnum.FULL_GIFT.name().equals(fullDiscount.resolveCompatibleActivityType())
                && Boolean.TRUE.equals(fullDiscount.getGiftFlag())) {
            collectMatchedGifts(matchedRule, fullDiscount, mergedGifts, legacyGiftSkuIds);
        }
        if (Boolean.TRUE.equals(fullDiscount.getPointFlag()) && fullDiscount.getPoint() != null) {
            int basePoint = cartVO.getGiftPoint() == null ? 0 : cartVO.getGiftPoint();
            cartVO.setGiftPoint(basePoint + fullDiscount.getPoint());
        }
        if (Boolean.TRUE.equals(fullDiscount.getFreeFreightFlag())) {
            for (CartSkuVO skuVO : cartVO.getCheckedSkuList()) {
                skuVO.setIsFreeFreight(true);
            }
        }
    }

    /**
     * 构建阶梯凑单提示文案
     *
     * @param price          当前已选活动商品金额
     * @param fullDiscountVO 满额活动 VO
     * @param ruleList       阶梯规则列表
     * @param matchedIndex   当前命中的最高阶梯下标（-1 表示未达首档）
     * @return 凑单提示文案
     */
    private String buildTierPromotionNotice(Double price,
                                            FullDiscountVO fullDiscountVO,
                                            List<FullDiscountRuleDTO> ruleList,
                                            int matchedIndex) {
        if (CollUtil.isEmpty(ruleList)) {
            return "";
        }
        String activityName = CharSequenceUtil.blankToDefault(fullDiscountVO.getPromotionName(), "满优惠");
        if (matchedIndex < 0) {
            FullDiscountRuleDTO firstRule = ruleList.get(0);
            Double diff = CurrencyUtil.sub(firstRule.getFullMoney(), price);
            return "还差" + diff + "元即可参与活动（" + activityName + "）" + fullDiscountVO.formatTierNotice(firstRule);
        }
        FullDiscountRuleDTO currentRule = ruleList.get(matchedIndex);
        if (matchedIndex + 1 < ruleList.size()) {
            FullDiscountRuleDTO nextRule = ruleList.get(matchedIndex + 1);
            Double diff = CurrencyUtil.sub(nextRule.getFullMoney(), price);
            return "正在参与活动[" + activityName + "]已享" + fullDiscountVO.formatTierNotice(currentRule)
                    + "，再买" + diff + "元可享" + fullDiscountVO.formatTierNotice(nextRule);
        }
        return "正在参与活动[" + activityName + "]已享" + fullDiscountVO.formatTierNotice(currentRule);
    }

    /**
     * 渲染满折优惠并记录价格扣减明细
     *
     * @param cart           店铺购物车
     * @param skuPriceDetail 参与满折的 SKU 价格 Map
     * @param rate           折扣率（如 8.5 折传入 0.85）
     * @param activityId     满折活动 ID
     */
    private void renderFullRate(CartVO cart, Map<String, Double> skuPriceDetail, Double rate, String activityId) {
        List<CartSkuVO> cartSkuVOS = cart.getCheckedSkuList().stream()
                .filter(Objects::nonNull)
                .filter(cartSkuVO -> skuPriceDetail.containsKey(cartSkuVO.getGoodsSku().getId()))
                .collect(Collectors.toList());

        cartSkuVOS.forEach(cartSkuVO -> {
            PriceDetailDTO priceDetailDTO = cartSkuVO.getPriceDetailDTO();
            Double discountPrice = CurrencyUtil.mul(
                    CurrencyUtil.sub(priceDetailDTO.getGoodsPrice(), priceDetailDTO.getPaidMemberSubsidy()),
                    CurrencyUtil.sub(1, rate)
            );
            priceDetailDTO.setDiscountPrice(
                    CurrencyUtil.add(priceDetailDTO.getDiscountPrice(), discountPrice)
            );
            priceDetailDTO.addDiscountPriceItem(DiscountPriceItem.builder()
                    .discountPrice(discountPrice)
                    .promotionTypeEnum(PromotionTypeEnum.FULL_DISCOUNT)
                    .promotionId(activityId)
                    .skuId(cartSkuVO.getGoodsSku().getId())
                    .goodsId(cartSkuVO.getGoodsSku().getGoodsId())
                    .build());
        });
    }

    /**
     * 统计参与满额活动的商品总价
     *
     * @param skuPriceMap SKU 价格 Map
     * @return 商品合计金额
     */
    private Double countPrice(Map<String, Double> skuPriceMap) {
        double count = 0d;
        for (Double price : skuPriceMap.values()) {
            count = CurrencyUtil.add(count, price);
        }
        return count;
    }
}
