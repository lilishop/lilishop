package cn.lili.message;

/**
 * 消息发送回调工厂。
 */
public final class MessageSendCallbackBuilder {

    private MessageSendCallbackBuilder() {
    }

    public static DefaultMessageSendCallback commonCallback() {
        return new DefaultMessageSendCallback();
    }
}
