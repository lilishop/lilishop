package cn.lili.modules.promotion.entity.enums;

/**
 * 优惠券获取方式枚举
 *
 * @author Chopper
 * @since 2020-03-19 9:36 上午
 */
public enum CouponGetEnum {

    /**
     * 枚举
     */
    FREE("免费获取"), ACTIVITY("活动获取");

    private final String description;

    CouponGetEnum(String str) {
        this.description = str;
    }

    public String description() {
        return description;
    }


}
