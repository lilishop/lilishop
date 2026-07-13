package cn.lili.message;

import cn.lili.common.properties.RocketmqCustomProperties;
import com.alibaba.fastjson2.JSON;
import jakarta.annotation.PreDestroy;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.event.EventListener;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * 后台 Redis 队列消费者，使用 BRPOP 持续拉取消息。
 */
@Slf4j
@Component
@ConditionalOnProperty(name = "lili.message.consumer.enabled", havingValue = "true")
public class RedisQueueConsumer {

    private final StringRedisTemplate stringRedisTemplate;
    private final RocketmqCustomProperties properties;
    private final MessageDispatcher messageDispatcher;
    private final ExecutorService executor = Executors.newFixedThreadPool(2);
    private final AtomicBoolean running = new AtomicBoolean(false);

    public RedisQueueConsumer(StringRedisTemplate stringRedisTemplate,
                              RocketmqCustomProperties properties,
                              MessageDispatcher messageDispatcher) {
        this.stringRedisTemplate = stringRedisTemplate;
        this.properties = properties;
        this.messageDispatcher = messageDispatcher;
    }

    @EventListener(ApplicationReadyEvent.class)
    public void start() {
        if (!running.compareAndSet(false, true)) {
            return;
        }
        List<String> queueKeys = buildQueueKeys();
        log.info("Redis queue consumer started, queues={}", queueKeys);
        executor.submit(() -> consumeLoop(queueKeys));
        executor.submit(this::processDelayedMessages);
    }

    @PreDestroy
    public void stop() {
        running.set(false);
        executor.shutdown();
        try {
            if (!executor.awaitTermination(5, TimeUnit.SECONDS)) {
                executor.shutdownNow();
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            executor.shutdownNow();
        }
    }

    private List<String> buildQueueKeys() {
        List<String> keys = new ArrayList<>();
        addQueueKey(keys, properties.getGoodsTopic());
        addQueueKey(keys, properties.getOrderTopic());
        addQueueKey(keys, properties.getMemberTopic());
        addQueueKey(keys, properties.getStoreTopic());
        addQueueKey(keys, properties.getAfterSaleTopic());
        addQueueKey(keys, properties.getNoticeTopic());
        addQueueKey(keys, properties.getNoticeSendTopic());
        addQueueKey(keys, properties.getPromotionTopic());
        return keys;
    }

    private void addQueueKey(List<String> keys, String topic) {
        if (topic != null && !topic.isBlank()) {
            keys.add(LiliMessageTemplate.QUEUE_PREFIX + topic);
        }
    }

    private void consumeLoop(List<String> queueKeys) {
        while (running.get()) {
            try {
                boolean consumed = false;
                for (String queueKey : queueKeys) {
                    String json = stringRedisTemplate.opsForList().rightPop(queueKey, Duration.ofSeconds(1));
                    if (json != null) {
                        dispatchJson(json);
                        consumed = true;
                        break;
                    }
                }
                if (!consumed) {
                    Thread.sleep(200);
                }
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                break;
            } catch (Exception e) {
                log.error("redis queue consume error", e);
                sleepQuietly(1000);
            }
        }
    }

    private void processDelayedMessages() {
        while (running.get()) {
            try {
                long now = Instant.now().getEpochSecond();
                Set<String> readyMessages = stringRedisTemplate.opsForZSet()
                        .rangeByScore(LiliMessageTemplate.DELAY_KEY, 0, now, 0, 20);
                if (readyMessages != null) {
                    for (String json : readyMessages) {
                        Long removed = stringRedisTemplate.opsForZSet().remove(LiliMessageTemplate.DELAY_KEY, json);
                        if (removed != null && removed > 0) {
                            QueueMessage message = JSON.parseObject(json, QueueMessage.class);
                            if (message != null && message.getTopic() != null) {
                                stringRedisTemplate.opsForList()
                                        .leftPush(LiliMessageTemplate.QUEUE_PREFIX + message.getTopic(), json);
                            }
                        }
                    }
                }
                Thread.sleep(1000);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                break;
            } catch (Exception e) {
                log.error("redis delayed queue error", e);
                sleepQuietly(1000);
            }
        }
    }

    private void dispatchJson(String json) {
        QueueMessage message = JSON.parseObject(json, QueueMessage.class);
        messageDispatcher.dispatch(message);
    }

    private void sleepQuietly(long millis) {
        try {
            Thread.sleep(millis);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }
}
