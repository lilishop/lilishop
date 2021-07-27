package cn.lili.modules.message.entity.enums;

/**
 * 消息发送类型
 *
 * @author pikachu
 * @since 2020/12/8 9:46
 */
public enum RangeEnum {

    //全部用户
    ALL("全部"),
    //指定用户
    APPOINT("指定用户");

    private final String description;

    RangeEnum(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }


}
