package cn.lili.modules.promotion.entity.enums;

/**
 * 优惠券活动发送类型枚举
 *
 * @author Bulbasaur
 * @since 2021/5/20 5:47 下午
 */
public enum CouponFrequencyEnum {

    /**
     * 领取周期
     */
    DAY("每天"), WEEK("每周"), MONTH("每月");

    private final String description;

    CouponFrequencyEnum(String str) {
        this.description = str;
    }

    public String description() {
        return description;
    }

    public static boolean exist(String name) {
        try {
            CouponFrequencyEnum.valueOf(name);
        } catch (IllegalArgumentException e) {
            return false;
        }
        return true;
    }

}
