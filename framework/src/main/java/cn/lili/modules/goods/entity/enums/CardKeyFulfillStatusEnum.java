package cn.lili.modules.goods.entity.enums;

/**
 * 电子卡券订单项卡密履约状态（订单详情扩展字段，非卡池 status）。
 *
 * @author Mike
 * @date 2026-08-02
 */
public enum CardKeyFulfillStatusEnum {

    /** 已支付/处理中，尚未交付卡密 */
    PENDING("待发放"),

    /** 已成功分配 ALLOCATED 卡密 */
    DELIVERED("已发放"),

    /** 发卡失败或子单已取消 */
    FAILED("发放失败"),

    /** 非电子卡券或不涉及卡池 */
    NOT_APPLICABLE("不适用");

    private final String description;

    CardKeyFulfillStatusEnum(String description) {
        this.description = description;
    }

    public String description() {
        return description;
    }
}
