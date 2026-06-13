package cn.lili.modules.payment.kit;

import cn.lili.common.utils.SnowFlake;
import cn.lili.common.utils.SpringContextUtil;
import cn.lili.modules.order.aftersale.entity.dos.AfterSale;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.order.service.StoreFlowService;
import cn.lili.modules.payment.entity.RefundLog;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.modules.promotion.service.GiftCardCashService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import jakarta.servlet.http.HttpServletRequest;
import java.math.BigDecimal;

/**
 * 退款支持
 *
 * @author Chopper
 * @since 2020-12-19 09:25
 */
@Component
@Slf4j
public class RefundSupport {
    /**
     * 店铺流水
     */
    @Autowired
    private StoreFlowService storeFlowService;
    /**
     * 订单
     */
    @Autowired
    private OrderService orderService;
    @Autowired
    private GiftCardCashService giftCardCashService;

    /**
     * 售后退款
     *
     * @param afterSale
     */
    public void refund(AfterSale afterSale) {
        Order order = orderService.getBySn(afterSale.getOrderSn());
        BigDecimal channelRefund = giftCardCashService.resolveChannelRefundAfterGiftCardForAfterSale(afterSale);
        double channelRefundAmount = channelRefund.doubleValue();
        if (channelRefundAmount <= 0D) {
            // 全额回卡，不调用第三方退款通道，仅记录退款流水
            storeFlowService.refundOrder(afterSale);
            return;
        }
        RefundLog refundLog = RefundLog.builder()
                .isRefund(false)
                .totalAmount(channelRefundAmount)
                .payPrice(channelRefundAmount)
                .memberId(afterSale.getMemberId())
                .paymentName(order.getPaymentMethod())
                .afterSaleNo(afterSale.getSn())
                .paymentReceivableNo(order.getReceivableNo())
                .outOrderNo("AF" + SnowFlake.getIdStr())
                .orderSn(afterSale.getOrderSn())
                .refundReason(afterSale.getReason())
                .build();
        PaymentMethodEnum paymentMethodEnum = PaymentMethodEnum.paymentNameOf(order.getPaymentMethod());
        Payment payment = (Payment) SpringContextUtil.getBean(paymentMethodEnum.getPlugin());
        payment.refund(refundLog);

        //记录退款流水
        storeFlowService.refundOrder(afterSale);
    }

    /**
     * 退款通知
     *
     * @param paymentMethodEnum 支付渠道
     */
    public void notify(PaymentMethodEnum paymentMethodEnum,
                       HttpServletRequest request) {

        //获取支付插件
        Payment payment = (Payment) SpringContextUtil.getBean(paymentMethodEnum.getPlugin());
        payment.refundNotify(request);
    }

}
