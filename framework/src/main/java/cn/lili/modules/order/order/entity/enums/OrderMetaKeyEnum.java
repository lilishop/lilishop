package cn.lili.modules.order.order.entity.enums;

/**
 * 订单元Key枚举
 *
 * @author Chopper
 * @since 2020/11/17 7:26 下午
 */
public enum OrderMetaKeyEnum {

    /**
     * 订单属性
     */
    POINT("使用的积分"),
    DISCOUNT_PRICE("优惠金额"),
    GIFT_POINT("赠送的积分"),
    GIFT_COUPON("赠送的优惠券"),
    GIFT_SKU("赠品");

    private final String description;

    OrderMetaKeyEnum(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }

    public String description() {
        return this.description;
    }


}
