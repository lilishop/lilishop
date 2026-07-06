package cn.lili.modules.live.entity.enums;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
public enum LiveStautsEnum {

    NEW("新直播"),
    LIVING("直播中"),
    PAUSED("已暂停"),
    ENDED("已结束");



    private final String description;

    LiveStautsEnum(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
}
