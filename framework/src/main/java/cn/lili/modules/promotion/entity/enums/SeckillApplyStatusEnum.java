package cn.lili.modules.promotion.entity.enums;

/**
 * 秒杀活动状态枚举
 *
 * @author paulG
 * @since 2020/8/26
 **/
public enum SeckillApplyStatusEnum {

    /**
     * 当前店铺对当前秒杀活动的申请状态
     */
    APPLIED("已经申请过"), NOT_APPLY("未报名"), EXPIRE("过期的");

    private final String description;

    SeckillApplyStatusEnum(String str) {
        this.description = str;
    }

    public String description() {
        return description;
    }

}
