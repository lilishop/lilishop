package cn.lili.modules.page.entity.enums;

/**
 * 功能反馈枚举
 *
 * @author Bulbasaur
 * @since 2020/12/7 10:50
 */
public enum FeedbackTypeEnum {

    /**
     * 功能建议
     */
    FUNCTION,

    /**
     * 优化反馈
     */
    OPTIMIZE ,

    /**
     * 其他意见
     */
    OTHER;

    public String value() {
        return this.name();
    }

}
