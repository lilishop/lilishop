package cn.lili.modules.promotion.entity.enums;

/**
 * 限时抢购状态枚举
 *
 * @author paulG
 * @date 2020/8/26
 **/
public enum SeckillApplyStatusEnum {

    /**
     * 当前店铺对当前限时抢购的申请状态
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
