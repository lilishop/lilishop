package cn.lili.modules.message.entity.enums;

/**
 * 消息标题
 *
 * @author pikachu
 * @since 2020/12/8 9:46
 */
public enum MessageTitle {

    /**
     * 您有新的订单，请您关注
     */
    NEW_ORDER("您有新的订单，请您关注"),
    /**
     * 您有订单被支付，请您及时进行发货处理
     */
    PAY_ORDER("您有订单被支付，请您及时进行发货处理");

    private final String description;

    MessageTitle(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }

}
