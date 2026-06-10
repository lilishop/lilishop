package cn.lili.config;

import cn.lili.rocketmq.local.LocalRocketMqTemplate;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * all-in-one 精简镜像不再内置 RocketMQ，通过本地事件总线复用原有 listener。
 */
@Configuration
public class LiteMessageQueueConfig {

    @Bean
    @ConditionalOnMissingBean(RocketMQTemplate.class)
    public RocketMQTemplate rocketMQTemplate(ApplicationEventPublisher applicationEventPublisher) {
        return new LocalRocketMqTemplate(applicationEventPublisher);
    }
}
