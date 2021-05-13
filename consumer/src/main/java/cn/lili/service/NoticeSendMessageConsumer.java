package cn.lili.service;

import cn.lili.event.OrderStatusChangeEvent;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import org.springframework.stereotype.Component;

/**
 * 消息发送
 *
 * @author paulG
 * @since 2020/12/9
 */
@Component
public class NoticeSendMessageConsumer implements OrderStatusChangeEvent {


    @Override
    public void orderChange(OrderMessage orderMessage) {


    }
}
