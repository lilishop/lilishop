package cn.lili.rocketmq.local;

import com.alibaba.fastjson2.JSON;
import org.apache.rocketmq.client.producer.SendCallback;
import org.apache.rocketmq.client.producer.SendResult;
import org.apache.rocketmq.client.producer.SendStatus;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.messaging.Message;

import java.nio.charset.StandardCharsets;

/**
 * 精简模式没有 RocketMQ Broker 时，将发送动作转成本地 Spring 事件。
 */
public class LocalRocketMqTemplate extends RocketMQTemplate {

    private final ApplicationEventPublisher applicationEventPublisher;

    public LocalRocketMqTemplate(ApplicationEventPublisher applicationEventPublisher) {
        this.applicationEventPublisher = applicationEventPublisher;
    }

    @Override
    public void asyncSend(String destination, Object payload, SendCallback sendCallback) {
        publish(destination, payload);
        if (sendCallback != null) {
            sendCallback.onSuccess(successResult());
        }
    }

    @Override
    public void asyncSend(String destination, Object payload, SendCallback sendCallback, long timeout) {
        asyncSend(destination, payload, sendCallback);
    }

    @Override
    public void asyncSend(String destination, Message<?> message, SendCallback sendCallback) {
        asyncSend(destination, (Object) message, sendCallback);
    }

    @Override
    public void asyncSend(String destination, Message<?> message, SendCallback sendCallback, long timeout) {
        asyncSend(destination, (Object) message, sendCallback);
    }

    @Override
    public SendResult syncSend(String destination, Object payload) {
        publish(destination, payload);
        return successResult();
    }

    @Override
    public SendResult syncSend(String destination, Message<?> message) {
        publish(destination, message);
        return successResult();
    }

    @Override
    public void sendOneWay(String destination, Object payload) {
        publish(destination, payload);
    }

    @Override
    public void sendOneWay(String destination, Message<?> message) {
        publish(destination, message);
    }

    @Override
    protected void doSend(String destination, Message<?> message) {
        publish(destination, message);
    }

    private void publish(String destination, Object payload) {
        Destination parsed = Destination.parse(destination);
        applicationEventPublisher.publishEvent(new LocalRocketMqMessage(parsed.topic(), parsed.tag(), toBody(payload)));
    }

    private byte[] toBody(Object payload) {
        Object realPayload = payload instanceof Message<?> message ? message.getPayload() : payload;
        if (realPayload == null) {
            return new byte[0];
        }
        if (realPayload instanceof byte[] bytes) {
            return bytes;
        }
        if (realPayload instanceof String text) {
            return text.getBytes(StandardCharsets.UTF_8);
        }
        return JSON.toJSONString(realPayload).getBytes(StandardCharsets.UTF_8);
    }

    private SendResult successResult() {
        SendResult sendResult = new SendResult();
        sendResult.setSendStatus(SendStatus.SEND_OK);
        return sendResult;
    }

    private record Destination(String topic, String tag) {
        private static Destination parse(String destination) {
            String[] parts = destination == null ? new String[]{""} : destination.split(":", 2);
            return new Destination(parts[0], parts.length > 1 ? parts[1] : null);
        }
    }
}
