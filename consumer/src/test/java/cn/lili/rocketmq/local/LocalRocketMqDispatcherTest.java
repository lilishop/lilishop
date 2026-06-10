package cn.lili.rocketmq.local;

import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.listener.GoodsMessageListener;
import cn.lili.listener.OrderMessageListener;
import cn.lili.trigger.TimeTriggerConsumer;
import cn.lili.trigger.model.TimeTriggerMsg;
import org.apache.rocketmq.common.message.MessageExt;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;

import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

class LocalRocketMqDispatcherTest {

    @Test
    void dispatchesMessageExtListenerByTopicAndTag() {
        GoodsMessageListener goodsMessageListener = mock(GoodsMessageListener.class);
        LocalRocketMqDispatcher dispatcher = new LocalRocketMqDispatcher(properties(), goodsMessageListener, mock(OrderMessageListener.class), null, null, null, null, null, null, null);

        dispatcher.dispatch(new LocalRocketMqMessage("goods-topic", "GOODS_AUDIT", "{\"id\":\"goods-1\"}".getBytes(StandardCharsets.UTF_8)));

        verify(goodsMessageListener).onMessage(argThat((MessageExt message) ->
                "goods-topic".equals(message.getTopic())
                        && "GOODS_AUDIT".equals(message.getTags())
                        && "{\"id\":\"goods-1\"}".equals(new String(message.getBody(), StandardCharsets.UTF_8))));
    }

    @Test
    void dispatchesTimeTriggerMessageByPromotionTopic() {
        TimeTriggerConsumer timeTriggerConsumer = mock(TimeTriggerConsumer.class);
        LocalRocketMqDispatcher dispatcher = new LocalRocketMqDispatcher(properties(), null, null, null, null, null, null, null, null, timeTriggerConsumer);

        dispatcher.dispatch(new LocalRocketMqMessage("promotion-topic", null, "{\"triggerExecutor\":\"coupon\"}".getBytes(StandardCharsets.UTF_8)));

        verify(timeTriggerConsumer).onMessage(argThat((TimeTriggerMsg message) -> "coupon".equals(message.getTriggerExecutor())));
    }

    private RocketmqCustomProperties properties() {
        RocketmqCustomProperties properties = new RocketmqCustomProperties();
        properties.setGoodsTopic("goods-topic");
        properties.setOrderTopic("order-topic");
        properties.setMemberTopic("member-topic");
        properties.setStoreTopic("store-topic");
        properties.setAfterSaleTopic("after-sale-topic");
        properties.setNoticeTopic("notice-topic");
        properties.setNoticeSendTopic("notice-send-topic");
        properties.setPromotionTopic("promotion-topic");
        return properties;
    }
}
