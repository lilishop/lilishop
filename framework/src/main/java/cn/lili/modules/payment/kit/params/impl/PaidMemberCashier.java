package cn.lili.modules.payment.kit.params.impl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.SnowFlake;
import cn.lili.common.utils.SpringContextUtil;
import cn.lili.modules.member.service.PaidMemberService;
import cn.lili.modules.payment.entity.RefundLog;
import cn.lili.modules.payment.entity.enums.CashierEnum;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.modules.payment.kit.Payment;
import cn.lili.modules.payment.kit.dto.PayParam;
import cn.lili.modules.payment.kit.dto.PaymentSuccessParams;
import cn.lili.modules.payment.kit.params.CashierExecute;
import cn.lili.modules.payment.kit.params.dto.CashierParam;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 付费会员复用现有收银台及已验签支付回调。
 * @author Bulbasaur
 * @since 2026-11-04
 */
@Slf4j
@Component
public class PaidMemberCashier implements CashierExecute {
    @Autowired private PaidMemberService service;
    @Override public CashierEnum cashierEnum() { return CashierEnum.PAID_MEMBER; }
    @Override public CashierParam getPaymentParams(PayParam param) {
        if (!cashierEnum().name().equals(param.getOrderType())) return null;
        var order = service.ownedOrder(UserContext.getCurrentUser().getId(), param.getSn());
        if (!"UNPAID".equals(order.getStatus()) || !order.getPayDeadline().after(new java.util.Date()))
            throw new ServiceException(ResultCode.PARAMS_ERROR, "会员订单已支付、取消或超时");
        CashierParam result = new CashierParam();
        result.setPrice(order.getAmount().doubleValue()); result.setTitle("付费会员开通/续费");
        result.setDetail(service.snapshot(order).getName()); result.setOrderSns(order.getSn()); result.setCreateTime(order.getCreateTime());
        result.setAutoCancel(order.getPayDeadline().getTime());
        return result;
    }
    @Override public void paymentSuccess(PaymentSuccessParams params) {
        if (cashierEnum().name().equals(params.getPayParam().getOrderType()))
            service.paid(params.getPayParam().getSn(), params.getPrice(), params.getPaymentMethod(), params.getReceivableNo());
    }
    @Override public Boolean paymentResult(PayParam param) {
        if (!cashierEnum().name().equals(param.getOrderType())) return false;
        var order = UserContext.getCurrentUser() == null ? service.order(param.getSn()) : service.ownedOrder(UserContext.getCurrentUser().getId(), param.getSn());
        return "PAID".equals(order.getStatus());
    }

    @Override
    public boolean isSamePayment(PaymentSuccessParams params) {
        if (!cashierEnum().name().equals(params.getPayParam().getOrderType())) return false;
        var order = UserContext.getCurrentUser() == null ? service.order(params.getPayParam().getSn()) : service.ownedOrder(UserContext.getCurrentUser().getId(), params.getPayParam().getSn());
        if (order != null && "PAID".equals(order.getStatus())) {
            return CharSequenceUtil.isNotEmpty(order.getReceivableNo())
                    && order.getReceivableNo().equals(params.getReceivableNo())
                    && params.getPaymentMethod() != null
                    && params.getPaymentMethod().equals(order.getPaymentMethod());
        }
        return false;
    }

    @Override
    public void refundDuplicatePayment(PaymentSuccessParams params) {
        if (!cashierEnum().name().equals(params.getPayParam().getOrderType())) return;
        var order = UserContext.getCurrentUser() == null ? service.order(params.getPayParam().getSn()) : service.ownedOrder(UserContext.getCurrentUser().getId(), params.getPayParam().getSn());
        if (order != null) {
            PaymentMethodEnum paymentMethodEnum = PaymentMethodEnum.paymentNameOf(params.getPaymentMethod());
            if (paymentMethodEnum != null) {
                RefundLog refundLog = RefundLog.builder()
                        .isRefund(false)
                        .totalAmount(order.getAmount().doubleValue())
                        .payPrice(order.getAmount().doubleValue())
                        .memberId(order.getMemberId())
                        .paymentName(params.getPaymentMethod())
                        .afterSaleNo("DUP_PAY_" + SnowFlake.getIdStr())
                        .paymentReceivableNo(params.getReceivableNo())
                        .outOrderNo("RF" + SnowFlake.getIdStr())
                        .orderSn(order.getSn())
                        .refundReason("付费会员订单重复支付，第二笔款项自动原路退回")
                        .build();
                try {
                    Payment payment = (Payment) SpringContextUtil.getBean(paymentMethodEnum.getPlugin());
                    if (payment != null) {
                        payment.refund(refundLog);
                    }
                } catch (Exception e) {
                    log.error("付费会员单[{}]重复付款原路退款失败：receivableNo={}", order.getSn(), params.getReceivableNo(), e);
                }
            }
        }
    }
}
