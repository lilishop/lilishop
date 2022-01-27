package cn.lili.modules.order.order.entity.enums;

/**
 * 订单状态枚举
 *
 * @author Chopper
 * @since 2020/11/17 7:27 下午
 */
public enum OrderStatusEnum {

    /**
     * 订单状态
     */
    UNPAID("未付款"),
    PAID("已付款"),
    UNDELIVERED("待发货"),
    DELIVERED("已发货"),
    COMPLETED("已完成"),
    /**
     * 虚拟订单需要核验商品
     */
    TAKE("待核验"),
    CANCELLED("已取消");

    private final String description;

    OrderStatusEnum(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }

    public String description() {
        return this.description;
    }


}
