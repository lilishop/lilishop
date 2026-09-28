package cn.lili.event.impl;

import cn.lili.event.OrderStatusChangeEvent;
import cn.lili.modules.appointment.service.AppointmentService;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.service.OrderService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 预约订单履约：支付成功后扣时段库存、生成工单与 N 条核销码，并流转至 TAKE。
 *
 * @author Bulbasaur
 * @since 2026-09-20
 */
@Slf4j
@Component
public class AppointmentOrderFulfillExecute implements OrderStatusChangeEvent {

    @Autowired
    private OrderService orderService;

    @Autowired
    private AppointmentService appointmentService;

    @Override
    public void orderChange(OrderMessage orderMessage) {
        if (orderMessage.getNewStatus() == null) {
            return;
        }
        Order order = orderService.getBySn(orderMessage.getOrderSn());
        if (order == null || !OrderTypeEnum.APPOINTMENT.name().equals(order.getOrderType())) {
            return;
        }
        if (orderMessage.getNewStatus() == OrderStatusEnum.PAID) {
            if (!PayStatusEnum.PAID.name().equals(order.getPayStatus())) {
                return;
            }
            log.info("预约订单 PAID 履约开始，orderSn={}", orderMessage.getOrderSn());
            appointmentService.fulfillAfterPayment(orderMessage.getOrderSn());
            return;
        }
        if (orderMessage.getNewStatus() == OrderStatusEnum.TAKE) {
            log.debug("预约订单已进入 TAKE，核销码已由 li_appointment_verification_code 管理，orderSn={}",
                    orderMessage.getOrderSn());
        }
    }
}
