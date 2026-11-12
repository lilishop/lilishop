package cn.lili.modules.order.order.listener;

import cn.lili.modules.order.order.event.OrderCancelledEvent;
import cn.lili.modules.order.order.event.OrderPaidEvent;
import cn.lili.modules.order.order.service.StoreFlowService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * 店铺财务订单流水领域事件监听器
 * <p>
 * 监听订单支付与取消领域事件，同步记录店铺付款流水及退款冲回流水，将流水逻辑与核心订单服务解耦。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Slf4j
@Component
public class StoreFlowOrderEventListener {

    @Autowired
    private StoreFlowService storeFlowService;

    /**
     * 监听订单支付事件，记录店铺支付交易流水
     *
     * @param event 订单支付成功事件
     */
    @EventListener
    public void onOrderPaid(OrderPaidEvent event) {
        if (event == null || event.getOrder() == null) {
            return;
        }
        log.info("接收到订单支付领域事件，开始记流水，orderSn={}", event.getOrder().getSn());
        storeFlowService.payOrder(event.getOrder());
    }

    /**
     * 监听订单取消事件，记录店铺退款冲回流水
     *
     * @param event 订单取消成功事件
     */
    @EventListener
    public void onOrderCancelled(OrderCancelledEvent event) {
        if (event == null || event.getOrder() == null) {
            return;
        }
        log.info("接收到订单取消领域事件，开始冲回流水，orderSn={}", event.getOrder().getSn());
        storeFlowService.orderCancel(event.getOrder());
    }
}
