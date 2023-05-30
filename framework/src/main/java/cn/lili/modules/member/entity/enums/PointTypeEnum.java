package cn.lili.modules.member.entity.enums;

/**
 * 积分类型枚举
 *
 * @author Chopper
 * @since 2021/3/20 10:44
 */

public enum PointTypeEnum {
    /**
     * 增加
     */
    INCREASE("增加"),
    /**
     * 减少
     */
    REDUCE("减少");

    private String description;

    public String description() {
        return description;
    }

    PointTypeEnum(String description) {
        this.description = description;
    }
}
