package cn.lili.listener;

import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.event.OrderStatusChangeEvent;
import cn.lili.event.TradeEvent;
import cn.lili.message.QueueMessage;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.rocketmq.tags.OrderTagsEnum;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * 订单消息监听器
 *
 * @author paulG
 * @since 2020/12/9
 **/
@Component
@Slf4j
public class OrderMessageListener {

    /**
     * 交易事件监听列表
     */
    @Autowired
    private List<TradeEvent> tradeEvent;
    /**
     * 订单状态改变事件监听列表
     */
    @Autowired
    private List<OrderStatusChangeEvent> orderStatusChangeEvents;
    /**
     * 缓存
     */
    @Autowired
    private Cache<Object> cache;

    public void onMessage(QueueMessage messageExt) {
        try {
            this.orderStatusEvent(messageExt);
        } catch (Exception e) {
            log.error("订单状态变更事件处理发生异常", e);
        }
    }

    /**
     * 订单状态变更事件派发
     *
     * @param messageExt 队列消息
     */
    public void orderStatusEvent(QueueMessage messageExt) {

        switch (OrderTagsEnum.valueOf(messageExt.getTags())) {
            // 创建订单
            case ORDER_CREATE:
                String key = new String(messageExt.getBody());
                String cacheContent = cache.getString(key);
                if (cacheContent == null) {
                    log.error("TradeDTO 缓存未命中，key: {}", key);
                    throw new RuntimeException("TradeDTO not found in cache: " + key);
                }
                TradeDTO tradeDTO = JSONUtil.toBean(cacheContent, TradeDTO.class);
                boolean result = true;
                for (TradeEvent event : tradeEvent) {
                    try {
                        event.orderCreate(tradeDTO);
                    } catch (Exception e) {
                        log.error("订单 [{}] 创建，执行器 [{}] 发生异常",
                                tradeDTO.getSn(),
                                event.getClass().getName(),
                                e);
                        result = false;
                    }
                }
                // 全部事件执行成功才清理缓存；若存在失败则抛出异常以便进入重试机制
                if (Boolean.TRUE.equals(result)) {
                    cache.remove(key);
                } else {
                    throw new RuntimeException("Order create trade event execution failed: " + tradeDTO.getSn());
                }
                break;
            // 订单状态改变
            case STATUS_CHANGE:
                for (OrderStatusChangeEvent orderStatusChangeEvent : orderStatusChangeEvents) {
                    try {
                        OrderMessage orderMessage = JSONUtil.toBean(new String(messageExt.getBody()), OrderMessage.class);
                        orderStatusChangeEvent.orderChange(orderMessage);
                    } catch (Exception e) {
                        log.error("订单消息 [{}]，事件处理器 [{}] 执行发生异常",
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
