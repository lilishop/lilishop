package cn.lili.modules.message.entity.enums;

/**
 * 消息发送客户端
 *
 * @author pikachu
 * @since 2020/12/8 9:46
 */
public enum MessageSendClient {

    //全部用户
    MEMBER("会员"),
    //指定用户
    STORE("店铺");

    private final String description;

    MessageSendClient(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }


}
