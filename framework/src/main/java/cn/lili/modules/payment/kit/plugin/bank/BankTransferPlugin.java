package cn.lili.modules.payment.kit.plugin.bank;

import cn.lili.common.enums.ClientTypeEnum;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.payment.entity.RefundLog;
import cn.lili.modules.payment.kit.Payment;
import cn.lili.modules.payment.kit.dto.PayParam;
import cn.lili.modules.payment.kit.dto.PaymentSuccessParams;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.modules.payment.kit.params.dto.CashierParam;
import cn.lili.modules.payment.service.PaymentService;
import cn.lili.modules.payment.service.RefundLogService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 线下收款
 *
 * @author Chopper
 * @version v1.0
 * 2021-02-20 10:14
 */
@Slf4j
@Component
public class BankTransferPlugin implements Payment {
    /**
     * 退款日志
     */
    @Autowired
    private RefundLogService refundLogService;
    /**
     * 支付日志
     */
    @Autowired
    private PaymentService paymentService;

    @Override
    public void refund(RefundLog refundLog) {
        try {
            refundLog.setIsRefund(true);
            refundLogService.save(refundLog);
        } catch (Exception e) {
            log.error("线下收款错误",e);
        }
    }

    /**
     * 支付订单
     *
     * @param order 订单
     */
    public void callBack(Order order) {

        //收银参数
        CashierParam cashierParam = new CashierParam();
        cashierParam.setPrice(order.getFlowPrice());
        //支付参数
        PayParam payParam = new PayParam();
        payParam.setOrderType("ORDER");
        payParam.setSn(order.getSn());
        payParam.setClientType(ClientTypeEnum.PC.name());

        PaymentSuccessParams paymentSuccessParams = new PaymentSuccessParams(
                PaymentMethodEnum.BANK_TRANSFER.name(),
                "",
                order.getFlowPrice(),
                payParam
        );

        //记录支付日志
        paymentService.adminPaySuccess(paymentSuccessParams);
        log.info("支付回调通知：线上支付：{}", payParam);
    }

}
