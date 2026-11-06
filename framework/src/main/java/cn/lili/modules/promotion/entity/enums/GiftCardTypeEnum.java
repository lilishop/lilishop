package cn.lili.modules.promotion.entity.enums;

/**
 * 礼品卡类型枚举（区分现金卡与提货卡；共用制卡/发卡，按类型区分金额抵扣与商品兑换）
 *
 * @author Bulbasaur
 * @since 2026-05-22
 */
public enum GiftCardTypeEnum {

    /** 现金卡（本期已实现） */
    CASH,
    /** 提货卡（一次兑换指定商品，取消不退权益） */
    PICKUP
}
