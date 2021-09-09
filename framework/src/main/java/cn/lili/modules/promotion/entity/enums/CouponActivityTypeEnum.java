package cn.lili.modules.promotion.entity.enums;

/**
 * 优惠券活动类型枚举
 *
 * @author Bulbasaur
 * @since 2021/5/20 5:47 下午
 */
public enum CouponActivityTypeEnum {

    /**
     * "新人赠券"
     */
    REGISTERED("新人赠券"),
    /**
     * "精确发券"
     */
    SPECIFY("精确发券");

    private final String description;

    CouponActivityTypeEnum(String str) {
        this.description = str;
    }

    public String description() {
        return description;
    }
}
