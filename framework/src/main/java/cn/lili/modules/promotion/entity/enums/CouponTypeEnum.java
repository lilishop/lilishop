package cn.lili.modules.promotion.entity.enums;

/**
 * 优惠券折扣类型
 *
 * @author Chopper
 * @since 2020-03-19 9:36 上午
 */
public enum CouponTypeEnum {

    /**
     * 枚举
     */
    DISCOUNT("打折"), PRICE("减免现金");

    private final String description;

    CouponTypeEnum(String str) {
        this.description = str;
    }

    public String description() {
        return description;
    }
}
