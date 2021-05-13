package cn.lili.modules.promotion.entity.enums;

/**
 * 促销活动申请状态枚举
 *
 * @author Chopper
 * @date 2020-03-19 9:36 上午
 */
public enum PromotionApplyStatusEnum {

    /**
     * 枚举
     */
    APPLY("申请"), PASS("通过"), REFUSE("拒绝");

    private final String description;

    PromotionApplyStatusEnum(String str) {
        this.description = str;
    }

    public String description() {
        return description;
    }
}
