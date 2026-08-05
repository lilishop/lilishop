package cn.lili.modules.distribution.entity.enums;

/**
 * 分销招募加入条件类型
 *
 * @author pikachu
 */
public enum RecruitJoinConditionTypeEnum {

    /** 无条件 */
    NONE("无条件"),

    /** 有条件 */
    CONDITIONAL("有条件");

    private final String description;

    RecruitJoinConditionTypeEnum(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
}
