package cn.lili.modules.message.entity.enums;

/**
 * 消息模板类型
 *
 * @author pikachu
 * @since 2020/12/8 9:46
 */
public enum MessageTemplateType {

    //会员消息
    MEMBER("会员消息"),
    //店铺消息
    STORE("店铺消息"),
    //其他消息
    OTHER("其他消息");

    private final String description;

    MessageTemplateType(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }


}
