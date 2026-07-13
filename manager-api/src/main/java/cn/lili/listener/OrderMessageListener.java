package cn.lili.listener;

import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.event.OrderStatusChangeEvent;
import cn.lili.event.TradeEvent;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.message.QueueMessage;
import cn.lili.rocketmq.tags.OrderTagsEnum;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * ????
 *
 * @author paulG
 * @since 2020/12/9
 **/
@Component
@Slf4j
public class OrderMessageListener {

    /**
     * ??
     */
    @Autowired
    private List<TradeEvent> tradeEvent;
    /**
     * ????
     */
    @Autowired
    private List<OrderStatusChangeEvent> orderStatusChangeEvents;
    /**
     * ??
     */
    @Autowired
    private Cache<Object> cache;

    public void onMessage(QueueMessage messageExt) {
        try {
            this.orderStatusEvent(messageExt);
        } catch (Exception e) {
            log.error("????????????", e);
        }
    }

    /**
     * ??????
     * @param messageExt
     */
    public void orderStatusEvent(QueueMessage messageExt) {

        switch (OrderTagsEnum.valueOf(messageExt.getTags())) {
            //????
            case ORDER_CREATE:
                String key = new String(messageExt.getBody());
                TradeDTO tradeDTO = JSONUtil.toBean(cache.getString(key), TradeDTO.class);
                boolean result = true;
                for (TradeEvent event : tradeEvent) {
                    try {
                        event.orderCreate(tradeDTO);
                    } catch (Exception e) {
                        log.error("??{}??,?{}??????????????",
                                tradeDTO.getSn(),
                                event.getClass().getName(),
                                e);
                        result = false;
                    }
                }
                //?????????
                if (Boolean.TRUE.equals(result)) {
                    //???????trade cache key
                    cache.remove(key);
                }
                break;
            //??????
            case STATUS_CHANGE:
                for (OrderStatusChangeEvent orderStatusChangeEvent : orderStatusChangeEvents) {
                    try {
                        OrderMessage orderMessage = JSONUtil.toBean(new String(messageExt.getBody()), OrderMessage.class);
                        orderStatusChangeEvent.orderChange(orderMessage);
                    } catch (Exception e) {
                        log.error("??{},?{}??????????????",
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
