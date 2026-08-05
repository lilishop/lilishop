package cn.lili.modules.distribution.entity.enums;

/**
 * 分销招募申请字段类型
 *
 * @author pikachu
 */
public enum DistributionRecruitFieldTypeEnum {

    /** 姓名 */
    NAME("姓名"),

    /** 手机号 */
    MOBILE("手机号"),

    /** 图片 */
    IMAGE("图片"),

    /** 性别 */
    GENDER("性别");

    private final String description;

    DistributionRecruitFieldTypeEnum(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
}
