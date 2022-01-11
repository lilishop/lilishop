package cn.lili.test.rocketmq;

import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.rocketmq.RocketmqSendCallbackBuilder;
import cn.lili.rocketmq.tags.OrderTagsEnum;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.messaging.Message;
import org.springframework.messaging.support.MessageBuilder;
import org.springframework.test.context.junit.jupiter.SpringExtension;

/**
 * @author paulG
 * @since 2021/1/15
 **/
@ExtendWith(SpringExtension.class)
@SpringBootTest
class MsgExtRocketMqTest {

    @Autowired
    private RocketMQTemplate rocketMQTemplate;

    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;

    @Test
    void searchAll() {
        String destination = rocketmqCustomProperties.getOrderTopic() + ":" + OrderTagsEnum.STATUS_CHANGE.name();
        Message<String> message = MessageBuilder.withPayload("Context").build();
        rocketMQTemplate.asyncSend(destination, message, RocketmqSendCallbackBuilder.commonCallback());
        rocketMQTemplate.send(destination, message);
        Assertions.assertTrue(true);
    }

}
