package cn.lili.listener;

import cn.hutool.json.JSONUtil;
import cn.lili.common.rocketmq.tags.AfterSaleTagsEnum;
import cn.lili.event.AfterSaleStatusChangeEvent;
import cn.lili.modules.order.order.entity.dos.AfterSale;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.common.message.MessageExt;
import org.apache.rocketmq.spring.annotation.RocketMQMessageListener;
import org.apache.rocketmq.spring.core.RocketMQListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * 售后通知
 *
 * @author paulG
 * @since 2020/12/9
 */
@Slf4j
@Component
@RocketMQMessageListener(topic = "${lili.data.rocketmq.after-sale-topic}", consumerGroup = "${lili.data.rocketmq.after-sale-group}")
public class AfterSaleMessageListener implements RocketMQListener<MessageExt> {

    /**
     * 售后订单状态
     */
    @Autowired
    private List<AfterSaleStatusChangeEvent> afterSaleStatusChangeEvents;

    @Override
    public void onMessage(MessageExt messageExt) {
        switch (AfterSaleTagsEnum.valueOf(messageExt.getTags())) {
            case AFTER_SALE_STATUS_CHANGE:
                for (AfterSaleStatusChangeEvent afterSaleStatusChangeEvent : afterSaleStatusChangeEvents) {
                    try {
                        AfterSale afterSale = JSONUtil.toBean(new String(messageExt.getBody()), AfterSale.class);
                        afterSaleStatusChangeEvent.afterSaleStatusChange(afterSale);
                    } catch (Exception e) {
                        log.error("售后{},在{}业务中，状态修改事件执行异常",
                                new String(messageExt.getBody()),
                                afterSaleStatusChangeEvent.getClass().getName(),
                                e);
                    }
                }
            default:
                log.error("售后状态修改事件执行异常：", new String(messageExt.getBody()));
                break;
        }

    }
}
