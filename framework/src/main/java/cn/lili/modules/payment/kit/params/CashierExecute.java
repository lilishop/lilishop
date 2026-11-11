package cn.lili.modules.payment.kit.params;

import cn.lili.modules.payment.kit.dto.PaymentSuccessParams;
import cn.lili.modules.payment.kit.dto.PayParam;
import cn.lili.modules.payment.entity.enums.CashierEnum;
import cn.lili.modules.payment.kit.params.dto.CashierParam;

/**
 * 收银台接口
 *
 * @author Chopper
 * @since 2021-01-25 19:08
 */
public interface CashierExecute {

    /**
     * 获取支付参数
     *
     * @param payParam 收银台支付参数
     * @return 收银台所需支付参数
     */
    CashierParam getPaymentParams(PayParam payParam);

    /**
     * 支付成功
     *
     * @param paymentSuccessParams 支付回调
     */
    void paymentSuccess(PaymentSuccessParams paymentSuccessParams);

    /**
     * 支付结果查询
     *
     * @param payParam
     * @return
     */
    Boolean paymentResult(PayParam payParam);

    /**
     * 服务的枚举类型
     *
     * @return
     */
    CashierEnum cashierEnum();

    /**
     * 判断是否为相同支付流水的幂等通知重发
     *
     * @param paymentSuccessParams 支付参数
     * @return true-相同流水重发，无需重复处理或退款；false-非相同流水
     */
    default boolean isSamePayment(PaymentSuccessParams paymentSuccessParams) {
        return false;
    }

    /**
     * 收到重复付款时的自动原路退款处理
     *
     * @param paymentSuccessParams 支付参数
     */
    default void refundDuplicatePayment(PaymentSuccessParams paymentSuccessParams) {
    }
}
