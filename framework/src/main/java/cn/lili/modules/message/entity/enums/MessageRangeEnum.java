package cn.lili.modules.message.entity.enums;

/**
 * 发送消息范围枚举
 *
 * @author pikachu
 * @since 2020/12/8 9:46
 */
public enum MessageRangeEnum {

    /**
     * 所有用户
     */
    ALL("所有用户"),
    /**
     * 指定用户
     */
    USER("指定用户");

    private final String description;

    MessageRangeEnum(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }


}
