package cn.lili.modules.payment.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.payment.entity.enums.CashierEnum;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.modules.payment.kit.CashierSupport;
import cn.lili.modules.payment.kit.dto.PayParam;
import cn.lili.modules.payment.kit.dto.PaymentSuccessParams;
import cn.lili.modules.payment.kit.params.CashierExecute;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 收银台重复支付及防重复扣款单元测试
 *
 * @author Bulbasaur
 * @since 2026-11-11
 */
class PaymentServiceImplDuplicatePaymentTest {

    private PaymentServiceImpl paymentService;
    private CashierSupport cashierSupport;
    private CashierExecute orderCashier;

    @BeforeEach
    void setUp() {
        paymentService = new PaymentServiceImpl();
        cashierSupport = mock(CashierSupport.class);
        orderCashier = mock(CashierExecute.class);

        when(orderCashier.cashierEnum()).thenReturn(CashierEnum.ORDER);

        ReflectionTestUtils.setField(paymentService, "cashierSupport", cashierSupport);
        ReflectionTestUtils.setField(paymentService, "cashierExecutes", List.of(orderCashier));
    }

    /**
     * 已支付订单再次使用余额支付时，必须抛出 PAY_DOUBLE_ERROR 异常以触发 WalletPlugin 回滚余额扣减
     */
    @Test
    @DisplayName("已支付订单重复余额支付必须抛出异常触发余额回滚")
    void duplicateWalletPaymentThrowsPayDoubleError() {
        PayParam payParam = new PayParam();
        payParam.setOrderType(CashierEnum.ORDER.name());
        payParam.setSn("O10001");

        PaymentSuccessParams params = new PaymentSuccessParams(
                PaymentMethodEnum.WALLET.name(),
                "",
                88.0,
                payParam
        );

        // 模拟订单已支付
        when(cashierSupport.paymentResult(payParam)).thenReturn(true);

        assertThatThrownBy(() -> paymentService.success(params))
                .isInstanceOf(ServiceException.class)
                .hasMessage(ResultCode.PAY_DOUBLE_ERROR.message());

        // 不应执行正常入账
        verify(orderCashier, never()).paymentSuccess(any());
        // 不走渠道退款（由 WalletPlugin 捕获业务异常手动 increase 返还）
        verify(orderCashier, never()).refundDuplicatePayment(any());
    }

    /**
     * 相同支付流水的重复通知属于幂等重发，忽略处理且不触发退款
     */
    @Test
    @DisplayName("相同支付流水幂等通知重发不触发退款")
    void idempotentNotificationRetryDoesNotRefund() {
        PayParam payParam = new PayParam();
        payParam.setOrderType(CashierEnum.ORDER.name());
        payParam.setSn("O10001");

        PaymentSuccessParams params = new PaymentSuccessParams(
                PaymentMethodEnum.WECHAT.name(),
                "wx_transaction_123",
                88.0,
                payParam
        );

        when(cashierSupport.paymentResult(payParam)).thenReturn(true);
        when(orderCashier.isSamePayment(params)).thenReturn(true);

        paymentService.success(params);

        // 幂等请求：既不重复入账，也不触发退款
        verify(orderCashier, never()).paymentSuccess(any());
        verify(orderCashier, never()).refundDuplicatePayment(any());
    }

    /**
     * 已支付订单收到第二笔不同流水（如支付宝二次付款）时，必须自动触发原路退款
     */
    @Test
    @DisplayName("已支付订单收到第二笔支付流水必须自动原路退款")
    void duplicateThirdPartyPaymentTriggersAutoRefund() {
        PayParam payParam = new PayParam();
        payParam.setOrderType(CashierEnum.ORDER.name());
        payParam.setSn("O10001");

        PaymentSuccessParams params = new PaymentSuccessParams(
                PaymentMethodEnum.ALIPAY.name(),
                "ali_trade_456",
                88.0,
                payParam
        );

        when(cashierSupport.paymentResult(payParam)).thenReturn(true);
        when(orderCashier.isSamePayment(params)).thenReturn(false);

        paymentService.success(params);

        // 验证自动触发了重复付款原路退款
        verify(orderCashier).refundDuplicatePayment(params);
        // 且未重复入账
        verify(orderCashier, never()).paymentSuccess(any());
    }
}
