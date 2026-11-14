package cn.lili.modules.order.order.support;

import cn.lili.common.security.OperationalJudgment;
import cn.lili.modules.order.order.aop.OrderLogPoint;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.mapper.OrderMapper;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

/**
 * 通用订单完成和系统取消的独立入口，供订单门面及预约业务复用，不依赖完整订单服务。
 * 沿用原有状态更新、退款分摊及消息发送规则；操作日志在此统一记录，兼容入口不重复记录。
 * 外部退款不具备数据库事务回滚能力，本次拆分不增加自动重试或独立补偿事务。
 *
 * @author Bulbasaur
 * @since 2026-11-14
 */
@Component
@RequiredArgsConstructor
public class OrderLifecycleSupport {

    private final OrderMapper orders;
    private final OrderStatusSupport statuses;

    /**
     * 校验当前操作人对订单的访问资格后完成订单，保留原有评价、投诉及销量消息处理。
     *
     * @param orderSn 订单编号
     */
    @Transactional(rollbackFor = Exception.class)
    @OrderLogPoint(description = "'订单['+#orderSn+']完成'", orderSn = "#orderSn")
    public void complete(String orderSn) {
        Order order = OperationalJudgment.judgment(orders.selectOne(new LambdaQueryWrapper<Order>().eq(Order::getSn, orderSn)));
        statuses.complete(order, orderSn);
    }

    /**
     * 系统流程完成订单，不执行操作人归属校验；调用方保留其原有业务资格校验。
     *
     * @param orderSn 订单编号
     */
    @Transactional(rollbackFor = Exception.class)
    @OrderLogPoint(description = "'订单['+#orderSn+']完成'", orderSn = "#orderSn")
    public void systemComplete(String orderSn) {
        Order order = orders.selectOne(new LambdaQueryWrapper<Order>().eq(Order::getSn, orderSn));
        statuses.complete(order, orderSn);
    }

    /**
     * 按原有条件更新及退款规则系统关单，重复取消不重复触发退款。
     * 不退款关单仅允许待支付订单；其他允许取消的状态由订单状态组件统一约束。
     *
     * @param orderSn 订单编号
     * @param reason 取消原因
     * @param refundMoney 是否执行退款流程
     */
    @Transactional(rollbackFor = Exception.class)
    @OrderLogPoint(description = "'订单['+#orderSn+']系统取消，原因为：'+#reason", orderSn = "#orderSn")
    public void systemCancel(String orderSn, String reason, Boolean refundMoney) {
        statuses.systemCancel(orderSn, reason, refundMoney);
    }

    /**
     * 预约履约成功后以条件更新将已支付订单推进为待核销，仅更新成功时发布提交后状态消息。
     * 参与调用方履约事务，不在此增加事务代理边界。
     *
     * @param orderSn 预约订单编号
     */
    public void transitionAppointmentToTake(String orderSn) {
        int updated = orders.update(null, new LambdaUpdateWrapper<Order>()
                .eq(Order::getSn, orderSn)
                .eq(Order::getOrderType, OrderTypeEnum.APPOINTMENT.name())
                .eq(Order::getOrderStatus, OrderStatusEnum.PAID.name())
                .set(Order::getOrderStatus, OrderStatusEnum.TAKE.name()));
        if (updated > 0) {
            OrderMessage message = new OrderMessage();
            message.setOrderSn(orderSn);
            message.setNewStatus(OrderStatusEnum.TAKE);
            statuses.sendUpdateStatusMessage(message);
        }
    }
}
