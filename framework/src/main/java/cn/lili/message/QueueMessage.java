package cn.lili.message;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Redis 队列消息体，替代 RocketMQ MessageExt。
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class QueueMessage {

    private String topic;

    private String tag;

    private byte[] body;

    /**
     * 消费重试次数计数
     */
    private Integer retryCount = 0;

    public QueueMessage(String topic, String tag, byte[] body) {
        this.topic = topic;
        this.tag = tag;
        this.body = body;
        this.retryCount = 0;
    }

    public String getTags() {
        return tag;
    }
}
