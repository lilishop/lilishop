package cn.lili.modules.order.order.event;

import cn.lili.modules.order.order.entity.dos.Order;
import lombok.Getter;
import org.springframework.context.ApplicationEvent;

/**
 * 订单支付成功领域事件
 * <p>
 * 当订单支付核心逻辑执行完成且状态成功流转为 PAID 时发布，通知店铺财务流水记账等下游监听者。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Getter
public class OrderPaidEvent extends ApplicationEvent {

    private static final long serialVersionUID = 1L;

    private final Order order;

    /**
     * 构造订单支付成功事件
     *
     * @param source 事件源
     * @param order  支付成功的订单实体
     */
    public OrderPaidEvent(Object source, Order order) {
        super(source);
        this.order = order;
    }
}
