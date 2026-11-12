package cn.lili.listener;

import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.message.LiliMessageTemplate;
import cn.lili.modules.stock.entity.dto.StockFlowMessage;
import cn.lili.modules.stock.event.StockFlowEvent;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.event.TransactionPhase;
import org.springframework.transaction.event.TransactionalEventListener;

/**
 * 库存流水事件桥接监听器
 * 监听业务层发布的 Spring StockFlowEvent，并在事务提交后将其桥接转发至 Redis/RocketMQ 队列
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Slf4j
@Component
public class StockFlowBridgeListener {

    @Autowired
    private LiliMessageTemplate liliMessageTemplate;

    @Autowired
    private RocketmqCustomProperties properties;

    /**
     * 事务提交后（或非事务环境下即时）将库存流水消息发送至队列
     *
     * @param event 库存流水应用事件
     */
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT, fallbackExecution = true)
    public void onStockFlowEvent(StockFlowEvent event) {
        if (event == null || event.getMessages() == null || event.getMessages().isEmpty()) {
            return;
        }
        String destination = properties.getStockFlowTopic();
        for (StockFlowMessage message : event.getMessages()) {
            try {
                liliMessageTemplate.sendOneWay(destination, message);
                log.debug("库存流水消息已投递队列: destination={}, skuId={}, flowType={}",
                        destination, message.getSkuId(), message.getFlowType());
            } catch (Exception e) {
                log.error("库存流水消息发送队列失败: destination={}, message={}", destination, message, e);
            }
        }
    }
}
