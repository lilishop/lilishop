package cn.lili.modules.order.order.event;

import cn.lili.modules.order.order.entity.dos.Order;
import lombok.Getter;
import org.springframework.context.ApplicationEvent;

/**
 * 订单取消成功领域事件
 * <p>
 * 当买家取消或系统自动关单成功，订单状态流转为 CANCELLED 后发布，驱动店铺退款流水冲回等下游监听者。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Getter
public class OrderCancelledEvent extends ApplicationEvent {

    private static final long serialVersionUID = 1L;

    private final Order order;

    private final String reason;

    /**
     * 构造订单取消成功事件
     *
     * @param source 事件源
     * @param order  已取消的订单实体
     * @param reason 取消原因
     */
    public OrderCancelledEvent(Object source, Order order, String reason) {
        super(source);
        this.order = order;
        this.reason = reason;
    }
}
