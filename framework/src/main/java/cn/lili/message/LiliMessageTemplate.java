package cn.lili.message;

import com.alibaba.fastjson2.JSON;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.messaging.Message;
import org.springframework.stereotype.Component;

import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.concurrent.TimeUnit;

/**
 * 基于 Redis List / ZSet 的跨进程消息队列，替代 RocketMQ。
 */
@Slf4j
@Component
public class LiliMessageTemplate {

    public static final String QUEUE_PREFIX = "lili:mq:queue:";

    public static final String DELAY_KEY = "lili:mq:delayed";

    /**
     * RocketMQ delay level 对应秒数（level 0 表示不延迟）。
     */
    private static final int[] DELAY_SECONDS = {
            0, 1, 5, 10, 30, 60, 120, 180, 240, 300, 360, 420, 480, 540, 600,
            1200, 1800, 3600, 7200
    };

    private final StringRedisTemplate stringRedisTemplate;

    public LiliMessageTemplate(StringRedisTemplate stringRedisTemplate) {
        this.stringRedisTemplate = stringRedisTemplate;
    }

    public void asyncSend(String destination, Object payload, MessageSendCallback sendCallback) {
        asyncSend(destination, payload, sendCallback, 3000);
    }

    public void asyncSend(String destination, Object payload, MessageSendCallback sendCallback, long timeout) {
        try {
            publish(destination, payload, null);
            if (sendCallback != null) {
                sendCallback.onSuccess(successResult());
            }
        } catch (Exception e) {
            log.error("async send failed, destination={}", destination, e);
            if (sendCallback != null) {
                sendCallback.onException(e);
            }
        }
    }

    public void asyncSend(String destination, Message<?> message, MessageSendCallback sendCallback) {
        asyncSend(destination, (Object) message, sendCallback);
    }

    public void asyncSend(String destination, Message<?> message, MessageSendCallback sendCallback, long timeout) {
        asyncSend(destination, (Object) message, sendCallback, timeout);
    }

    public SendResult syncSend(String destination, Object payload) {
        publish(destination, payload, null);
        return successResult();
    }

    public SendResult syncSend(String destination, Message<?> message) {
        publish(destination, message, null);
        return successResult();
    }

    public SendResult syncSend(String destination, Object payload, long timeout, int delayLevel) {
        publish(destination, payload, delayLevelToSeconds(delayLevel));
        return successResult();
    }

    public void sendOneWay(String destination, Object payload) {
        publish(destination, payload, null);
    }

    public void sendOneWay(String destination, Message<?> message) {
        publish(destination, message, null);
    }

    private void publish(String destination, Object payload, Long delaySeconds) {
        Destination parsed = Destination.parse(destination);
        QueueMessage queueMessage = new QueueMessage(
                parsed.topic(),
                parsed.tag(),
                toBody(payload)
        );
        String json = JSON.toJSONString(queueMessage);
        if (delaySeconds != null && delaySeconds > 0) {
            double score = Instant.now().getEpochSecond() + delaySeconds;
            stringRedisTemplate.opsForZSet().add(DELAY_KEY, json, score);
            log.debug("queued delayed message topic={}, tag={}, delaySeconds={}", parsed.topic(), parsed.tag(), delaySeconds);
            return;
        }
        stringRedisTemplate.opsForList().leftPush(QUEUE_PREFIX + parsed.topic(), json);
        log.debug("queued message topic={}, tag={}", parsed.topic(), parsed.tag());
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

    private long delayLevelToSeconds(int delayLevel) {
        if (delayLevel <= 0) {
            return 0;
        }
        if (delayLevel < DELAY_SECONDS.length) {
            return DELAY_SECONDS[delayLevel];
        }
        return DELAY_SECONDS[DELAY_SECONDS.length - 1];
    }

    private record Destination(String topic, String tag) {
        private static Destination parse(String destination) {
            String[] parts = destination == null ? new String[]{""} : destination.split(":", 2);
            return new Destination(parts[0], parts.length > 1 ? parts[1] : null);
        }
    }
}
