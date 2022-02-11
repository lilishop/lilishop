package cn.lili.modules.promotion.entity.enums;

/**
 * 优惠券时间范围枚举
 *
 * @author Bulbasaur
 * @since 2021/5/24 8:31 上午
 */
public enum CouponRangeDayEnum {

    /**
     * 枚举
     */
    FIXEDTIME("固定时间"), DYNAMICTIME("动态时间");

    private final String description;

    CouponRangeDayEnum(String str) {
        this.description = str;
    }

    public String description() {
        return description;
    }


}
