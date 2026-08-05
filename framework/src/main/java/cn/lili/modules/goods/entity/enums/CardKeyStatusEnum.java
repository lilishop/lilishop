package cn.lili.modules.goods.entity.enums;

/**
 * 卡密池状态
 *
 * @author Mike
 * @date 2026-07-31
 */
public enum CardKeyStatusEnum {

    /** 在库可售 */
    UNUSED("在库可售"),

    /** 拼团等场景预占，未成团前不可售 */
    RESERVED("预占"),

    /** 已售且已交付 */
    ALLOCATED("已售"),

    /** 作废 */
    VOIDED("作废");

    private final String description;

    CardKeyStatusEnum(String description) {
        this.description = description;
    }

    public String description() {
        return description;
    }
}
