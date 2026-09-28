package cn.lili.modules.order.order.entity.enums;

/**
 * 前端订单页面TAB标签枚举
 *
 * @author Chopper
 * @since 2020/11/17 7:28 下午
 */
public enum OrderTagEnum {


    /**
     * 所有订单
     */
    ALL("全部"),

    /**
     * 待付款
     */
    WAIT_PAY("待付款"),

    /**
     * 待收货
     */
    WAIT_SHIP("待发货"),

    /**
     * 待收货
     */
    WAIT_ROG("待收货"),

    /**
     * 待收货/使用：已发货待收货，以及虚拟订单待核验、自提订单待核销。
     */
    WAIT_USE("待收货/使用"),

    /**
     * 已完成
     */
    COMPLETE("已完成"),

    /**
     * 已取消
     */
    CANCELLED("已取消");

    private final String description;


    OrderTagEnum(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }

    public static OrderTagEnum defaultType() {
        return ALL;
    }


}
