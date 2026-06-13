package cn.lili.modules.promotion.entity.enums;

/**
 * 礼品卡类型枚举（区分现金卡与提货卡；当前版本仅实现现金卡制卡/发卡）
 *
 * @author Bulbasaur
 * @since 2026-05-22
 */
public enum GiftCardTypeEnum {

    /** 现金卡（本期已实现） */
    CASH,
    /** 提货卡（本期未实现，接口会拒绝） */
    PICKUP
}
