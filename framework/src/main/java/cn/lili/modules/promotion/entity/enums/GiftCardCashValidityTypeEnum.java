package cn.lili.modules.promotion.entity.enums;

/**
 * 礼品卡有效期类型枚举
 *
 * @author Bulbasaur
 * @since 2026-05-22
 */
public enum GiftCardCashValidityTypeEnum {

    /** 长期有效 */
    LONG_TERM,
    /** 固定到期日（实例上带 validEndTime） */
    FIXED_UNTIL,
    /** 激活后 N 个月内有效（制卡时 validEndTime 为空，激活时再算） */
    AFTER_ACTIVATE_MONTHS
}
