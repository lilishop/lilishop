package cn.lili.modules.live.entity.enums;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
public enum LiveMessageAuthStatusEnum {
    /**
     * 需要审核 并且待审核
     */
    TOBEAUDITED("待审核"),
    /**
     * 审核通过
     */
    PASS("审核通过"),
    /**
     * 审核通过
     */
    REFUSE("审核拒绝");

    private final String description;

    LiveMessageAuthStatusEnum(String description) {
        this.description = description;
    }

    public String description() {
        return description;
    }
}
