package cn.lili.modules.order.order.entity.enums;

/**
 * 订单可申请售后状态枚举
 *
 * @author Chopper
 * @since 2020/11/17 7:26 下午
 */
public enum OrderItemAfterSaleStatusEnum {

    /**
     * 订单申请售后状态
     */
    NEW("新订单，不能申请售后"),
    NOT_APPLIED("未申请"),
    ALREADY_APPLIED("已申请"),
    EXPIRED("已失效不允许申请售后"),
    PART_AFTER_SALE("部分售后");



    private final String description;

    OrderItemAfterSaleStatusEnum(String description) {
        this.description = description;
    }

    public String description() {
        return this.description;
    }

}
