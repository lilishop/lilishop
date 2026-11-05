package cn.lili.modules.member.entity.enums;

import cn.hutool.core.text.CharSequenceUtil;

import java.util.Arrays;

/**
 * 权益类型
 *
 * @author Bulbasaur
 * @since 2026-11-04
 */
public enum MemberBenefitTypeEnum {
    GRADE_BADGE("等级徽章"),
    POINT_MULTIPLIER("积分倍率"),
    EXCLUSIVE_CUSTOMER_SERVICE("专属客服"),
    COUPON_PACKAGE("券礼包"),
    GIFT_POINT("赠送积分"),
    CUSTOM("自定义");

    private final String description;

    MemberBenefitTypeEnum(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }

    public String value() {
        return this.name();
    }

    public static boolean isValid(String type) {
        if (CharSequenceUtil.isBlank(type)) {
            return false;
        }
        return Arrays.stream(MemberBenefitTypeEnum.values()).anyMatch(v -> v.name().equals(type));
    }
}
