package cn.lili.message;

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
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.nio.charset.StandardCharsets;

/**
 * 将 Redis 队列消息路由到原 listener 业务逻辑。
 */
@Slf4j
@Component
public class MessageDispatcher {

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

    public MessageDispatcher(RocketmqCustomProperties properties,
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

    public void dispatch(QueueMessage message) {
        if (message == null || message.getTopic() == null) {
            return;
        }
        try {
            if (topicEquals(message, properties.getGoodsTopic())) {
                goodsMessageListener.onMessage(message);
                wxChannelsGoodsSyncListener.onMessage(message);
                return;
            }
            if (topicEquals(message, properties.getOrderTopic())) {
                orderMessageListener.onMessage(message);
                return;
            }
            if (topicEquals(message, properties.getMemberTopic())) {
                memberMessageListener.onMessage(message);
                return;
            }
            if (topicEquals(message, properties.getStoreTopic())) {
                storeMessageListener.onMessage(message);
                return;
            }
            if (topicEquals(message, properties.getAfterSaleTopic())) {
                afterSaleMessageListener.onMessage(message);
                return;
            }
            if (topicEquals(message, properties.getNoticeTopic())) {
                noticeMessageListener.onMessage(message);
                return;
            }
            if (topicEquals(message, properties.getNoticeSendTopic())) {
                noticeSendMessageListener.onMessage(message);
                return;
            }
            if (topicEquals(message, properties.getPromotionTopic())) {
                timeTriggerConsumer.onMessage(
                        JSON.parseObject(new String(message.getBody(), StandardCharsets.UTF_8), TimeTriggerMsg.class)
                );
            }
        } catch (Exception e) {
            log.error("dispatch message failed, topic={}, tag={}", message.getTopic(), message.getTag(), e);
        }
    }

    private boolean topicEquals(QueueMessage message, String topic) {
        return topic != null && topic.equals(message.getTopic());
    }
}
