package cn.lili.modules.distribution.entity.enums;

/**
 * 分销招募申请方式
 *
 * @author pikachu
 */
public enum RecruitApplyModeEnum {

    /** 手动申请 */
    MANUAL("手动申请"),

    /** 满足条件自动申请 */
    AUTO("自动申请");

    private final String description;

    RecruitApplyModeEnum(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
}
