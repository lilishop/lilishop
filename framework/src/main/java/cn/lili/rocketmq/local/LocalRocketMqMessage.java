package cn.lili.rocketmq.local;

/**
 * 精简模式下的本地 MQ 消息事件，用 topic/tag/body 保持 RocketMQ listener 的入参语义。
 */
public record LocalRocketMqMessage(String topic, String tag, byte[] body) {
}
