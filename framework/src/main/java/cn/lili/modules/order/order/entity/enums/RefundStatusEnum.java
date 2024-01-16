package cn.lili.modules.order.order.entity.enums;

/**
 * 退款状态枚举
 *
 * @author Lele
 * @since 2024-1-5 10:59:22
 */
public enum RefundStatusEnum {

    /**
     * 退款状态
     */
    ALL_REFUND("全部退款"),
    PART_REFUND("部分退款"),
    NO_REFUND("未退款"),
    REFUNDING("退款中");

    private final String description;

    RefundStatusEnum(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }

    public String description() {
        return this.description;
    }


}
