package cn.lili.listener;

import cn.lili.message.QueueMessage;
import cn.lili.modules.stock.entity.dto.StockFlowMessage;
import cn.lili.modules.stock.service.StockFlowService;
import com.alibaba.fastjson2.JSON;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.nio.charset.StandardCharsets;

/**
 * 库存流水队列消息监听器
 * 消费来自 Redis 队列的库存流水消息并写入数据库台账，依赖唯一索引实现幂等去重
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Slf4j
@Component
public class StockFlowMessageListener {

    @Autowired
    private StockFlowService stockFlowService;

    /**
     * 处理库存流水消息
     *
     * @param message 队列消息
     */
    public void onMessage(QueueMessage message) {
        if (message == null || message.getBody() == null) {
            return;
        }
        try {
            String json = new String(message.getBody(), StandardCharsets.UTF_8);
            StockFlowMessage stockFlowMessage = JSON.parseObject(json, StockFlowMessage.class);
            if (stockFlowMessage != null) {
                stockFlowService.saveFlowFromMessage(stockFlowMessage);
            }
        } catch (Exception e) {
            log.error("库存流水消息消费异常: topic={}, tags={}", message.getTopic(), message.getTags(), e);
            throw e;
        }
    }
}
