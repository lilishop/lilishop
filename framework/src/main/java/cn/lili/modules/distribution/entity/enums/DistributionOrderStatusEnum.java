package cn.lili.modules.distribution.entity.enums;

/**
 * 分销员订单状态
 *
 * @author pikachu
 */
public enum DistributionOrderStatusEnum {

    /** 未完成 */
    NO_COMPLETED("未完成"),

    /** 已完成 */
    COMPLETE("完成"),

    /** 已退款 */
    REFUND("退款");

    private final String description;

    DistributionOrderStatusEnum(String description) {
        this.description = description;
    }
}
