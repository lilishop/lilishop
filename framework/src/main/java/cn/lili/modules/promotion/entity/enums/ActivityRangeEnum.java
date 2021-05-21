package cn.lili.modules.promotion.entity.enums;

/**
 * 优惠券活动范围枚举
 *
 * @author Bulbasaur
 * @date: 2021/5/20 6:05 下午
 */
public enum ActivityRangeEnum {

    ALL("新人赠券"),
    DESIGNATED("精确发券");

    private final String description;

    ActivityRangeEnum(String str) {
        this.description = str;
    }

    public String description() {
        return description;
    }
}
