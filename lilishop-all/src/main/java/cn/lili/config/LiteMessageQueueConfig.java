package cn.lili.config;

import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * all-in-one 精简镜像不再内置 RocketMQ，这里只提供占位 Bean 以满足既有业务服务注入。
 * 涉及真实异步消息投递的业务路径后续应替换为本地事件或 Redis 队列实现。
 */
@Configuration
public class LiteMessageQueueConfig {

    @Bean
    @ConditionalOnMissingBean(RocketMQTemplate.class)
    public RocketMQTemplate rocketMQTemplate() {
        return new RocketMQTemplate();
    }
}
