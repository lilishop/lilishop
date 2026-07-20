package cn.lili.modules.member.entity.enums;

/**
 * 积分发放/变动来源枚举
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
public enum PointSourceEnum {

    REGISTER("用户注册"),
    COMMENT("客户评价"),
    ORDER("订单支付/下单"),
    FULL_DISCOUNT("订单满优惠"),
    BENEFIT("等级权益"),
    SIGN("签到"),
    REFUND_BACK("退款回退"),
    ADMIN("后台调整"),
    ORDER_PAY("订单积分扣减"),
    UNKNOWN("未知");

    private final String description;

    PointSourceEnum(String description) {
        this.description = description;
    }

    public String description() {
        return description;
    }
}
