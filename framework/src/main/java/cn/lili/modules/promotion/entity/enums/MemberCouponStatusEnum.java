package cn.lili.modules.promotion.entity.enums;

/**
 * 会员优惠券状态枚举
 *
 * @author Chopper
 * @since 2020-03-19 9:36 上午
 */
public enum MemberCouponStatusEnum {
    /**
     * 枚举
     */
    NEW("领取"), USED("已使用"), EXPIRE("过期"), CLOSED("作废");

    private final String description;

    MemberCouponStatusEnum(String str) {
        this.description = str;
    }

    public String description() {
        return description;
    }
}
