package cn.lili.modules.order.order.listener;

import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.event.OrderCancelledEvent;
import cn.lili.modules.order.order.event.OrderPaidEvent;
import cn.lili.modules.order.order.service.StoreFlowService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.Mockito.verify;

/**
 * 订单领域事件监听器单元测试
 * <p>
 * 验证订单支付和取消领域事件能正确驱动店铺资金流水记录与冲回
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@ExtendWith(MockitoExtension.class)
class StoreFlowOrderEventListenerTest {

    @Mock
    private StoreFlowService storeFlowService;

    @InjectMocks
    private StoreFlowOrderEventListener storeFlowOrderEventListener;

    @Test
    @DisplayName("测试订单支付事件触发店铺入账流水")
    void testOnOrderPaid() {
        Order order = new Order();
        order.setSn("O_PAID_001");
        OrderPaidEvent event = new OrderPaidEvent(this, order);

        storeFlowOrderEventListener.onOrderPaid(event);

        verify(storeFlowService).payOrder(order);
    }

    @Test
    @DisplayName("测试订单取消事件触发店铺退款流水冲回")
    void testOnOrderCancelled() {
        Order order = new Order();
        order.setSn("O_CANCEL_001");
        OrderCancelledEvent event = new OrderCancelledEvent(this, order, "测试关单");

        storeFlowOrderEventListener.onOrderCancelled(event);

        verify(storeFlowService).orderCancel(order);
    }
}
