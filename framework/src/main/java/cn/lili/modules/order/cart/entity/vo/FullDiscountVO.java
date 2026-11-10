package cn.lili.modules.order.cart.entity.vo;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.text.CharSequenceUtil;
import com.alibaba.fastjson2.JSON;
import cn.lili.modules.promotion.entity.dos.FullDiscount;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.dto.FullDiscountGiftItem;
import cn.lili.modules.promotion.entity.dto.FullDiscountRuleDTO;
import cn.lili.modules.promotion.entity.enums.FullDiscountTypeEnum;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.springframework.beans.BeanUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * 满额活动（满减 / 满折 / 满赠）展示与交互 VO
 *
 * @author Chopper
 * @since 2020-04-01 10:42 上午
 */
@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
public class FullDiscountVO extends FullDiscount {

    private static final long serialVersionUID = -2330552735874105354L;

    @Schema(description = "促销关联的商品列表")
    private List<PromotionGoods> promotionGoodsList;

    @Schema(description = "阶梯规则列表（1~3级）")
    private List<FullDiscountRuleDTO> ruleList;

    @Schema(description = "赠品skuId")
    private String giftSkuId;

    @Schema(description = "赠品名称")
    private String giftSkuName;

    @Schema(description = "赠品路径")
    private String giftSkuThumbnail;

    public FullDiscountVO(FullDiscount fullDiscount) {
        if (fullDiscount != null) {
            BeanUtils.copyProperties(fullDiscount, this);
            this.initFromEntity();
        }
    }

    /**
     * 从实体属性初始化活动子类型及阶梯规则列表（兼容历史单门槛数据与新 JSON 阶梯数据）
     */
    public void initFromEntity() {
        if (CharSequenceUtil.isBlank(this.getActivityType())) {
            this.setActivityType(resolveCompatibleActivityType());
        }
        if (CollUtil.isEmpty(this.ruleList) && CharSequenceUtil.isNotBlank(this.getRulesJson())) {
            try {
                this.ruleList = JSON.parseArray(this.getRulesJson(), FullDiscountRuleDTO.class);
            } catch (Exception ignored) {
                this.ruleList = new ArrayList<>();
            }
        }
        if (CollUtil.isEmpty(this.ruleList) && this.getFullMoney() != null) {
            FullDiscountRuleDTO legacyRule = new FullDiscountRuleDTO();
            legacyRule.setFullMoney(this.getFullMoney());
            legacyRule.setFullMinus(this.getFullMinus());
            legacyRule.setFullRate(this.getFullRate());
            if (Boolean.TRUE.equals(this.getGiftFlag()) && CharSequenceUtil.isNotBlank(this.getGiftId())) {
                List<FullDiscountGiftItem> items = new ArrayList<>();
                for (String id : this.getGiftId().split(",")) {
                    if (CharSequenceUtil.isNotBlank(id)) {
                        items.add(FullDiscountGiftItem.builder()
                                .skuId(id.trim())
                                .goodsName(this.giftSkuName)
                                .thumbnail(this.giftSkuThumbnail)
                                .num(1)
                                .build());
                    }
                }
                legacyRule.setGiftItems(items);
            }
            this.ruleList = new ArrayList<>(Collections.singletonList(legacyRule));
        }
        if (CollUtil.isNotEmpty(this.ruleList)) {
            this.ruleList.sort(Comparator.comparing(
                    r -> r.getFullMoney() == null ? 0D : r.getFullMoney()));
        }
    }

    /**
     * 根据旧版标志位推断活动子类型编码
     *
     * @return 活动子类型编码（FULL_MINUS / FULL_RATE / FULL_GIFT）
     */
    public String resolveCompatibleActivityType() {
        if (FullDiscountTypeEnum.isValid(this.getActivityType())) {
            return this.getActivityType();
        }
        if (Boolean.TRUE.equals(this.getFullRateFlag())) {
            return FullDiscountTypeEnum.FULL_RATE.name();
        }
        if (!Boolean.TRUE.equals(this.getFullMinusFlag()) && Boolean.TRUE.equals(this.getGiftFlag())) {
            return FullDiscountTypeEnum.FULL_GIFT.name();
        }
        return FullDiscountTypeEnum.FULL_MINUS.name();
    }

    /**
     * 获取当前活动有效的阶梯规则列表（按门槛金额升序排列）
     *
     * @return 阶梯规则列表
     */
    public List<FullDiscountRuleDTO> resolveRuleList() {
        if (CollUtil.isEmpty(this.ruleList)) {
            this.initFromEntity();
        }
        return this.ruleList == null ? Collections.emptyList() : this.ruleList;
    }

    /**
     * 格式化单级阶梯的优惠描述文本
     *
     * @param rule 单级阶梯规则
     * @return 阶梯优惠描述，如 "满100元减10元"、"满200元打8.5折"、"满100元赠[商品A x2]"
     */
    public String formatTierNotice(FullDiscountRuleDTO rule) {
        if (rule == null || rule.getFullMoney() == null) {
            return "";
        }
        String type = resolveCompatibleActivityType();
        StringBuilder sb = new StringBuilder();
        sb.append("满").append(stripTrailingZeros(rule.getFullMoney())).append("元");
        if (FullDiscountTypeEnum.FULL_RATE.name().equals(type) && rule.getFullRate() != null) {
            sb.append("打").append(stripTrailingZeros(rule.getFullRate())).append("折");
        } else if (FullDiscountTypeEnum.FULL_GIFT.name().equals(type)) {
            if (CollUtil.isNotEmpty(rule.getGiftItems())) {
                String giftText = rule.getGiftItems().stream()
                        .filter(Objects::nonNull)
                        .map(item -> {
                            String name = CharSequenceUtil.isNotBlank(item.getGoodsName())
                                    ? item.getGoodsName()
                                    : (CharSequenceUtil.isNotBlank(this.giftSkuName) ? this.giftSkuName : "赠品");
                            int count = item.validNum();
                            return count > 1 ? name + "×" + count : name;
                        })
                        .collect(Collectors.joining("、"));
                sb.append("赠[").append(giftText).append("]");
            } else if (CharSequenceUtil.isNotBlank(this.giftSkuName)) {
                sb.append("赠[").append(this.giftSkuName).append("]");
            } else {
                sb.append("享满赠好礼");
            }
        } else if (rule.getFullMinus() != null) {
            sb.append("减").append(stripTrailingZeros(rule.getFullMinus())).append("元");
        }
        return sb.toString();
    }

    /**
     * 生成完整活动的优惠描述文案（多阶梯时以分号连接）
     *
     * @return 优惠提示文本
     */
    public String notice() {
        List<FullDiscountRuleDTO> rules = resolveRuleList();
        if (CollUtil.isNotEmpty(rules)) {
            String tierText = rules.stream()
                    .map(this::formatTierNotice)
                    .filter(CharSequenceUtil::isNotBlank)
                    .collect(Collectors.joining("；"));
            StringBuilder extra = new StringBuilder();
            if (Boolean.TRUE.equals(this.getFreeFreightFlag())) {
                extra.append(" 免运费");
            }
            if (Boolean.TRUE.equals(this.getPointFlag()) && this.getPoint() != null && this.getPoint() > 0) {
                extra.append(" 赠").append(this.getPoint()).append("积分");
            }
            if (Boolean.TRUE.equals(this.getCouponFlag())) {
                extra.append(" 赠优惠券");
            }
            return " " + tierText + extra;
        }
        StringBuilder stringBuffer = new StringBuilder();
        if (Boolean.TRUE.equals(this.getFullMinusFlag())) {
            stringBuffer.append(" 减").append(this.getFullMinus()).append("元 ");
        }
        if (Boolean.TRUE.equals(this.getFullRateFlag())) {
            stringBuffer.append(" 打").append(this.getFullRate()).append("折 ");
        }
        if (Boolean.TRUE.equals(this.getFreeFreightFlag())) {
            stringBuffer.append(" 免运费 ");
        }
        if (Boolean.TRUE.equals(this.getPointFlag())) {
            stringBuffer.append(" 赠").append(this.getPoint()).append("积分 ");
        }
        if (Boolean.TRUE.equals(this.getCouponFlag())) {
            stringBuffer.append(" 赠").append("优惠券 ");
        }
        if (Boolean.TRUE.equals(this.getGiftFlag() && CharSequenceUtil.isNotEmpty(giftSkuName))) {
            stringBuffer.append(" 赠品[").append(giftSkuName).append("]");
        }
        return stringBuffer.toString();
    }

    /**
     * 去除浮点数末尾无意义的 .0
     *
     * @param val 浮点数值
     * @return 格式化后的数值字符串
     */
    private static String stripTrailingZeros(Double val) {
        if (val == null) {
            return "0";
        }
        if (val == val.longValue()) {
            return String.valueOf(val.longValue());
        }
        return String.valueOf(val);
    }
}
