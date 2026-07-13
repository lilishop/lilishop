package cn.lili.message;

import lombok.extern.slf4j.Slf4j;

/**
 * 默认异步消息发送回调，记录入队结果。
 */
@Slf4j
public class DefaultMessageSendCallback implements MessageSendCallback {

    @Override
    public void onSuccess(SendResult sendResult) {
        log.info("async message sent, result={}", sendResult);
    }

    @Override
    public void onException(Throwable throwable) {
        log.error("async message send failed", throwable);
    }
}
