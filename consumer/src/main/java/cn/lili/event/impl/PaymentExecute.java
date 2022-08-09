package cn.lili.event.impl;

import cn.lili.common.utils.SnowFlake;
import cn.lili.common.utils.SpringContextUtil;
import cn.lili.event.OrderStatusChangeEvent;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.payment.entity.RefundLog;
import cn.lili.modules.payment.kit.Payment;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * 支付
 *
 * @author Chopper
 * @since 2021-03-13 16:58
 */
@Slf4j
@Service
public class PaymentExecute implements OrderStatusChangeEvent {

    /**
     * 订单
     */
    @Autowired
    private OrderService orderService;

    @Override
    public void orderChange(OrderMessage orderMessage) {

        if (orderMessage.getNewStatus() == OrderStatusEnum.CANCELLED) {
            Order order = orderService.getBySn(orderMessage.getOrderSn());

            //如果未付款，则不去要退回相关代码执行
            if (order.getPayStatus().equals(PayStatusEnum.UNPAID.name())) {
                return;
            }
            PaymentMethodEnum paymentMethodEnum = PaymentMethodEnum.valueOf(order.getPaymentMethod());

            //获取支付方式
            Payment payment =
                    (Payment) SpringContextUtil.getBean(paymentMethodEnum.getPlugin());

            RefundLog refundLog = RefundLog.builder()
                    .isRefund(false)
                    .totalAmount(order.getFlowPrice())
                    .payPrice(order.getFlowPrice())
                    .memberId(order.getMemberId())
                    .paymentName(order.getPaymentMethod())
                    .afterSaleNo("订单取消")
                    .orderSn(order.getSn())
                    .paymentReceivableNo(order.getReceivableNo())
                    .outOrderNo("AF" + SnowFlake.getIdStr())
                    .refundReason("订单取消")
                    .build();
            payment.refund(refundLog);
        }
    }
}
