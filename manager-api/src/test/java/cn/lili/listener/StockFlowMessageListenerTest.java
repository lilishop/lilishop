package cn.lili.listener;

import cn.lili.message.QueueMessage;
import cn.lili.modules.stock.entity.dto.StockFlowMessage;
import cn.lili.modules.stock.entity.enums.StockFlowDirectionEnum;
import cn.lili.modules.stock.entity.enums.StockFlowTypeEnum;
import cn.lili.modules.stock.service.StockFlowService;
import com.alibaba.fastjson2.JSON;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 商品库存流水消息监听器单元测试，覆盖正常入库流水反序列化与调用、空消息安全防御。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@ExtendWith(MockitoExtension.class)
class StockFlowMessageListenerTest {

    @InjectMocks
    private StockFlowMessageListener listener;

    @Mock
    private StockFlowService stockFlowService;

    /**
     * 验证正常消息到达时正确反序列化并调用持久化服务。
     */
    @Test
    void onMessageNormalMessageSavesSuccessfully() {
        StockFlowMessage flowMessage = new StockFlowMessage();
        flowMessage.setBizSn("SF202611120001");
        flowMessage.setStoreId("store-1");
        flowMessage.setGoodsId("goods-1");
        flowMessage.setSkuId("sku-1");
        flowMessage.setQuantity(5);
        flowMessage.setDirection(StockFlowDirectionEnum.IN.name());
        flowMessage.setFlowType(StockFlowTypeEnum.PROCUREMENT_INBOUND.name());

        QueueMessage queueMessage = new QueueMessage();
        queueMessage.setTopic("STOCK_FLOW_TOPIC");
        queueMessage.setBody(JSON.toJSONBytes(flowMessage));

        assertDoesNotThrow(() -> listener.onMessage(queueMessage));
        verify(stockFlowService).saveFlowFromMessage(any(StockFlowMessage.class));
    }

    /**
     * 验证空消息安全防御，不执行任何持久化操作。
     */
    @Test
    void onMessageNullMessageIgnored() {
        assertDoesNotThrow(() -> listener.onMessage(null));

        QueueMessage emptyBodyMessage = new QueueMessage();
        emptyBodyMessage.setBody(null);
        assertDoesNotThrow(() -> listener.onMessage(emptyBodyMessage));

        verify(stockFlowService, never()).saveFlowFromMessage(any());
    }
}
