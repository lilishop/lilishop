package cn.lili.modules.message.entity.enums;

/**
 * 发送消息范围
 *
 * @author pikachu
 * @date 2020/12/8 9:46
 */
public enum MessageRangeEnum {


    ALL("所有用户"),

    USER("指定用户");

    private final String description;

    MessageRangeEnum(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }


}
