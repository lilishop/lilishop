package cn.lili.event.impl;

import cn.lili.common.utils.SnowFlake;
import cn.lili.common.utils.SpringContextUtil;
import cn.lili.event.OrderStatusChangeEvent;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.payment.entity.RefundLog;
import cn.lili.modules.payment.kit.Payment;
import cn.lili.modules.payment.kit.enums.PaymentMethodEnum;
import cn.lili.modules.payment.service.PaymentService;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * 支付
 *
 * @author Chopper
 * @date 2021-03-13 16:58
 */
@Service
public class PaymentExecute implements OrderStatusChangeEvent {

    //支付日志
    @Autowired
    private PaymentService paymentService;
    //订单
    @Autowired
    private OrderService orderService;

    @Override
    public void orderChange(OrderMessage orderMessage) {

        switch (orderMessage.getNewStatus()) {
            case CANCELLED:
                Order order = orderService.getBySn(orderMessage.getOrderSn());
                //未付款不做处理 直接返回
                if (order.getPayStatus() == PayStatusEnum.UNPAID.name()) {
                    return;
                }

                PaymentMethodEnum paymentMethodEnum = PaymentMethodEnum.valueOf(order.getPaymentMethod());
                //进行退款操作
                switch (paymentMethodEnum) {
                    case WALLET:
                    case ALIPAY:
                    case WECHAT:
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
                                .outOrderNo("AF" + SnowFlake.getIdStr())
                                .refundReason("订单取消")
                                .build();
                        payment.cancel(refundLog);
                        break;
                    case BANK_TRANSFER:
                        break;
                }
            default:
                break;
        }


    }


}
