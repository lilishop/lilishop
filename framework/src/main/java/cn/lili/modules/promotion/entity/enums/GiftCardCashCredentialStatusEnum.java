package cn.lili.modules.promotion.entity.enums;

/**
 * 礼品卡卡密凭证状态枚举（平台制卡/库存）
 *
 * @author Bulbasaur
 * @since 2026-05-22
 */
public enum GiftCardCashCredentialStatusEnum {

    /** 未发放给会员 */
    UNISSUED,
    /** 已绑定用户礼品卡 */
    BOUND,
    /** 作废 */
    VOIDED,
    /** 过期（可由任务或业务更新） */
    EXPIRED
}
