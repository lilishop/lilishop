package cn.lili.rocketmq.local;

import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.listener.AfterSaleMessageListener;
import cn.lili.listener.GoodsMessageListener;
import cn.lili.listener.MemberMessageListener;
import cn.lili.listener.NoticeMessageListener;
import cn.lili.listener.NoticeSendMessageListener;
import cn.lili.listener.OrderMessageListener;
import cn.lili.listener.StoreMessageListener;
import cn.lili.listener.WxChannelsGoodsSyncListener;
import cn.lili.trigger.TimeTriggerConsumer;
import cn.lili.trigger.model.TimeTriggerMsg;
import com.alibaba.fastjson2.JSON;
import org.apache.rocketmq.common.message.MessageExt;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

import java.nio.charset.StandardCharsets;

/**
 * 精简模式下的本地 MQ 路由器，复用原 RocketMQ listener 的业务处理逻辑。
 */
@Component
@ConditionalOnProperty(value = "lili.rocketmq.local.enabled", havingValue = "true", matchIfMissing = true)
public class LocalRocketMqDispatcher {

    private final RocketmqCustomProperties properties;
    private final GoodsMessageListener goodsMessageListener;
    private final OrderMessageListener orderMessageListener;
    private final MemberMessageListener memberMessageListener;
    private final StoreMessageListener storeMessageListener;
    private final AfterSaleMessageListener afterSaleMessageListener;
    private final NoticeMessageListener noticeMessageListener;
    private final NoticeSendMessageListener noticeSendMessageListener;
    private final WxChannelsGoodsSyncListener wxChannelsGoodsSyncListener;
    private final TimeTriggerConsumer timeTriggerConsumer;

    public LocalRocketMqDispatcher(RocketmqCustomProperties properties,
                                   GoodsMessageListener goodsMessageListener,
                                   OrderMessageListener orderMessageListener,
                                   MemberMessageListener memberMessageListener,
                                   StoreMessageListener storeMessageListener,
                                   AfterSaleMessageListener afterSaleMessageListener,
                                   NoticeMessageListener noticeMessageListener,
                                   NoticeSendMessageListener noticeSendMessageListener,
                                   WxChannelsGoodsSyncListener wxChannelsGoodsSyncListener,
                                   TimeTriggerConsumer timeTriggerConsumer) {
        this.properties = properties;
        this.goodsMessageListener = goodsMessageListener;
        this.orderMessageListener = orderMessageListener;
        this.memberMessageListener = memberMessageListener;
        this.storeMessageListener = storeMessageListener;
        this.afterSaleMessageListener = afterSaleMessageListener;
        this.noticeMessageListener = noticeMessageListener;
        this.noticeSendMessageListener = noticeSendMessageListener;
        this.wxChannelsGoodsSyncListener = wxChannelsGoodsSyncListener;
        this.timeTriggerConsumer = timeTriggerConsumer;
    }

    @EventListener
    public void dispatch(LocalRocketMqMessage message) {
        if (topicEquals(message, properties.getGoodsTopic())) {
            dispatchMessageExt(goodsMessageListener, message);
            dispatchMessageExt(wxChannelsGoodsSyncListener, message);
            return;
        }
        if (topicEquals(message, properties.getOrderTopic())) {
            dispatchMessageExt(orderMessageListener, message);
            return;
        }
        if (topicEquals(message, properties.getMemberTopic())) {
            dispatchMessageExt(memberMessageListener, message);
            return;
        }
        if (topicEquals(message, properties.getStoreTopic())) {
            dispatchMessageExt(storeMessageListener, message);
            return;
        }
        if (topicEquals(message, properties.getAfterSaleTopic())) {
            dispatchMessageExt(afterSaleMessageListener, message);
            return;
        }
        if (topicEquals(message, properties.getNoticeTopic())) {
            dispatchMessageExt(noticeMessageListener, message);
            return;
        }
        if (topicEquals(message, properties.getNoticeSendTopic())) {
            dispatchMessageExt(noticeSendMessageListener, message);
            return;
        }
        if (topicEquals(message, properties.getPromotionTopic()) && timeTriggerConsumer != null) {
            timeTriggerConsumer.onMessage(JSON.parseObject(new String(message.body(), StandardCharsets.UTF_8), TimeTriggerMsg.class));
        }
    }

    private boolean topicEquals(LocalRocketMqMessage message, String topic) {
        return topic != null && topic.equals(message.topic());
    }

    private void dispatchMessageExt(org.apache.rocketmq.spring.core.RocketMQListener<MessageExt> listener, LocalRocketMqMessage message) {
        if (listener == null) {
            return;
        }
        MessageExt messageExt = new MessageExt();
        messageExt.setTopic(message.topic());
        messageExt.setTags(message.tag());
        messageExt.setBody(message.body());
        listener.onMessage(messageExt);
    }
}
