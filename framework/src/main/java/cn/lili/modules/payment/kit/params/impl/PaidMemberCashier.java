package cn.lili.modules.payment.kit.params.impl;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.modules.member.service.PaidMemberService;
import cn.lili.modules.payment.entity.enums.CashierEnum;
import cn.lili.modules.payment.kit.dto.PayParam;
import cn.lili.modules.payment.kit.dto.PaymentSuccessParams;
import cn.lili.modules.payment.kit.params.CashierExecute;
import cn.lili.modules.payment.kit.params.dto.CashierParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
/**
 * 付费会员复用现有收银台及已验签支付回调。
 * @author Bulbasaur
 * @since 2026-11-04
 */
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
}
