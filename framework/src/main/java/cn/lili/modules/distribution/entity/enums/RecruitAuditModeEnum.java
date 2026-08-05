package cn.lili.modules.distribution.entity.enums;

/**
 * 分销招募审核方式
 *
 * @author pikachu
 */
public enum RecruitAuditModeEnum {

    /** 人工审核 */
    MANUAL("人工审核"),

    /** 无需审核，满足条件自动通过 */
    AUTO_PASS("无需审核（满足条件自动通过）");

    private final String description;

    RecruitAuditModeEnum(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
}
