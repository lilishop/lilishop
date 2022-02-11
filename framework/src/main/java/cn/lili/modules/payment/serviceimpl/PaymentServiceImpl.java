package cn.lili.modules.payment.serviceimpl;

import cn.lili.modules.payment.kit.CashierSupport;
import cn.lili.modules.payment.kit.dto.PaymentSuccessParams;
import cn.lili.modules.payment.kit.params.CashierExecute;
import cn.lili.modules.payment.service.PaymentService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

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

        boolean paymentResult = cashierSupport.paymentResult(paymentSuccessParams.getPayParam());
        if (paymentResult) {
            log.warn("订单支付状态后，调用支付成功接口，流水号：{}", paymentSuccessParams.getReceivableNo());
            return;
        }

        log.debug("支付成功，第三方流水：{}", paymentSuccessParams.getReceivableNo());
        //支付结果处理
        for (CashierExecute cashierExecute : cashierExecutes) {
            cashierExecute.paymentSuccess(paymentSuccessParams);
        }
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