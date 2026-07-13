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

    public String getTags() {
        return tag;
    }
}
