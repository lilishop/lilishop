package cn.lili.modules.payment.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.modules.payment.kit.CashierSupport;
import cn.lili.modules.payment.kit.dto.PaymentSuccessParams;
import cn.lili.modules.payment.kit.params.CashierExecute;
import cn.lili.modules.payment.service.PaymentService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * 支付日志 业务实现
 *
 * @author Chopper
 * @since 2020-12-19 09:25
 */
@Slf4j
@Service
public class PaymentServiceImpl implements PaymentService {

    @Autowired
    private List<CashierExecute> cashierExecutes;
    @Autowired
    private CashierSupport cashierSupport;

    @Override
    public void success(PaymentSuccessParams paymentSuccessParams) {

        //支付状态
        boolean paymentResult = cashierSupport.paymentResult(paymentSuccessParams.getPayParam());

        //已支付则属于重复通知或重复支付
        if (paymentResult) {
            handleDuplicatePayment(paymentSuccessParams);
            return;
        }

        log.debug("支付成功，第三方流水：{}", paymentSuccessParams.getReceivableNo());
        //支付结果处理
        for (CashierExecute cashierExecute : cashierExecutes) {
            cashierExecute.paymentSuccess(paymentSuccessParams);
        }
    }

    /**
     * 处理已支付订单收到重复付款或重复通知的场景
     *
     * @param paymentSuccessParams 支付回调参数
     */
    private void handleDuplicatePayment(PaymentSuccessParams paymentSuccessParams) {
        // 1. 如果是余额支付，直接抛出异常触发 WalletPlugin 回滚余额扣减
        if (PaymentMethodEnum.WALLET.name().equals(paymentSuccessParams.getPaymentMethod())) {
            log.warn("收银台检测到重复支付（余额支付）：orderType={}, sn={}, 抛出异常触发余额返还",
                    paymentSuccessParams.getPayParam().getOrderType(),
                    paymentSuccessParams.getPayParam().getSn());
            throw new ServiceException(ResultCode.PAY_DOUBLE_ERROR);
        }

        // 2. 查找匹配的收银台处理器，区分幂等重发与重复支付原路退款
        for (CashierExecute cashierExecute : cashierExecutes) {
            if (cashierExecute.cashierEnum().name().equals(paymentSuccessParams.getPayParam().getOrderType())) {
                if (cashierExecute.isSamePayment(paymentSuccessParams)) {
                    log.info("收银台收到已入账订单幂等重发通知，忽略处理：orderType={}, sn={}, 流水号={}",
                            paymentSuccessParams.getPayParam().getOrderType(),
                            paymentSuccessParams.getPayParam().getSn(),
                            paymentSuccessParams.getReceivableNo());
                    return;
                }
                log.warn("收银台检测到重复付款：orderType={}, sn={}, 支付方式={}, 流水号={}，触发原路自动退款",
                        paymentSuccessParams.getPayParam().getOrderType(),
                        paymentSuccessParams.getPayParam().getSn(),
                        paymentSuccessParams.getPaymentMethod(),
                        paymentSuccessParams.getReceivableNo());
                cashierExecute.refundDuplicatePayment(paymentSuccessParams);
                return;
            }
        }
        log.warn("收银台重复收款且未匹配到处理器，流水号：{}", paymentSuccessParams.getReceivableNo());
    }

    @Override
    public void adminPaySuccess(PaymentSuccessParams paymentSuccessParams) {

        log.debug("支付状态修改成功->银行转账");
        //支付结果处理
        for (CashierExecute cashierExecute : cashierExecutes) {
            cashierExecute.paymentSuccess(paymentSuccessParams);
        }
    }
}
