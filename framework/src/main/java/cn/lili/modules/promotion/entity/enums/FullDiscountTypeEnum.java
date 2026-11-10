package cn.lili.modules.promotion.entity.enums;

import cn.hutool.core.text.CharSequenceUtil;

/**
 * 满额活动子类型枚举
 *
 * @author Bulbasaur
 * @since 2026-11-09
 */
public enum FullDiscountTypeEnum {

    FULL_MINUS("满减活动", "满减"),
    FULL_RATE("满折活动", "满折"),
    FULL_GIFT("满赠活动", "满赠");

    private final String description;
    private final String tag;

    FullDiscountTypeEnum(String description, String tag) {
        this.description = description;
        this.tag = tag;
    }

    public String description() {
        return description;
    }

    public String tag() {
        return tag;
    }

    public static boolean isValid(String code) {
        if (CharSequenceUtil.isBlank(code)) {
            return false;
        }
        for (FullDiscountTypeEnum item : values()) {
            if (item.name().equals(code)) {
                return true;
            }
        }
        return false;
    }

    public static FullDiscountTypeEnum fromCode(String code) {
        if (CharSequenceUtil.isBlank(code)) {
            return null;
        }
        for (FullDiscountTypeEnum item : values()) {
            if (item.name().equals(code)) {
                return item;
            }
        }
        return null;
    }
}
