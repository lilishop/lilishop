package cn.lili.listener;

import cn.hutool.json.JSONUtil;
import cn.lili.common.cache.Cache;
import cn.lili.common.rocketmq.tags.MqOrderTagsEnum;
import cn.lili.event.OrderStatusChangeEvent;
import cn.lili.event.TradeEvent;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.common.message.MessageExt;
import org.apache.rocketmq.spring.annotation.RocketMQMessageListener;
import org.apache.rocketmq.spring.core.RocketMQListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * 订单消息
 *
 * @author paulG
 * @since 2020/12/9
 **/
@Component
@Slf4j
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
@RocketMQMessageListener(topic = "${lili.data.rocketmq.order-topic}", consumerGroup = "${lili.data.rocketmq.order-group}")
public class OrderMessageListener implements RocketMQListener<MessageExt> {

    //交易
    private final List<TradeEvent> tradeEvent;
    //订单状态
    private final List<OrderStatusChangeEvent> orderStatusChangeEvents;
    //缓存
    private final Cache<Object> cache;

    @Override
    public void onMessage(MessageExt messageExt) {
        switch (MqOrderTagsEnum.valueOf(messageExt.getTags())) {
            //订单创建
            case ORDER_CREATE:
                String key = new String(messageExt.getBody());
                TradeDTO tradeDTO = (TradeDTO) cache.get(key);
                for (TradeEvent event : tradeEvent) {
                    try {
                        event.orderCreate(tradeDTO);
                    } catch (Exception e) {
                        log.error("交易{}入库,在{}业务中，状态修改事件执行异常",
                                tradeDTO.getSn(),
                                event.getClass().getName(),
                                e);
                    }
                }
                break;
            //订单状态变更
            case STATUS_CHANGE:
                for (OrderStatusChangeEvent orderStatusChangeEvent : orderStatusChangeEvents) {
                    try {
                        OrderMessage orderMessage = JSONUtil.toBean(new String(messageExt.getBody()), OrderMessage.class);
                        orderStatusChangeEvent.orderChange(orderMessage);
                    } catch (Exception e) {
                        log.error("订单{},在{}业务中，状态修改事件执行异常",
                                new String(messageExt.getBody()),
                                orderStatusChangeEvent.getClass().getName(),
                                e);
                    }
                }
                break;
            default:
                break;
        }
    }

}
