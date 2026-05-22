package cn.lili.modules.payment.kit.plugin.nopay;

import cn.lili.common.enums.ClientTypeEnum;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.payment.entity.RefundLog;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.modules.payment.kit.Payment;
import cn.lili.modules.payment.kit.dto.PayParam;
import cn.lili.modules.payment.kit.dto.PaymentSuccessParams;
import cn.lili.modules.payment.kit.params.dto.CashierParam;
import cn.lili.modules.payment.service.PaymentService;
import cn.lili.modules.payment.service.RefundLogService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 未实际支付插件（礼品卡/优惠券等抵扣后应付现金为 0，无需走第三方支付通道）
 *
 * @author Bulbasaur
 * @since 2026-05-22
 */
@Slf4j
@Component
public class NotActuallyPaidPlugin implements Payment {

    @Autowired
    private RefundLogService refundLogService;
    @Autowired
    private PaymentService paymentService;

    @Override
    public void refund(RefundLog refundLog) {
        try {
            refundLog.setIsRefund(true);
            refundLogService.save(refundLog);
        } catch (Exception e) {
            log.error("未实际支付订单退款记录失败", e);
        }
    }

    /**
     * 确认支付（无第三方回调，直接标记支付成功）
     *
     * @param order 订单
     */
    public void callBack(Order order) {
        PayParam payParam = new PayParam();
        payParam.setOrderType("ORDER");
        payParam.setSn(order.getSn());
        payParam.setClientType(ClientTypeEnum.PC.name());

        PaymentSuccessParams paymentSuccessParams = new PaymentSuccessParams(
                PaymentMethodEnum.NOT_ACTUALLY_PAID.name(),
                "",
                order.getFlowPrice() == null ? 0D : order.getFlowPrice(),
                payParam
        );

        paymentService.adminPaySuccess(paymentSuccessParams);
        log.info("未实际支付订单确认支付：{}", payParam.getSn());
    }

}
