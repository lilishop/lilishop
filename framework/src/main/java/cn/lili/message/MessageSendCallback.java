package cn.lili.message;

/**
 * 异步消息发送回调。
 */
public interface MessageSendCallback {

    void onSuccess(SendResult sendResult);

    void onException(Throwable throwable);
}
