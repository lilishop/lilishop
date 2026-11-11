package cn.lili.timetask.handler.impl.order;

import cn.lili.common.exception.ServiceException;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.modules.payment.kit.CashierSupport;
import cn.lili.modules.payment.kit.dto.PaymentQueryResult;
import cn.lili.modules.payment.service.PaymentService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 超时关单补偿查单逻辑测试
 * 验证关单前渠道主动查单、已支付触发补单防误关单、未支付先关渠道再取消系统订单、异常跳过保护机制
 *
 * @author Bulbasaur
 * @since 2026-11-11
 */
class CancelOrderTaskExecuteCompensationTest {

    private CancelOrderTaskExecute taskExecute;
    private OrderService orderService;
    private CashierSupport cashierSupport;
    private PaymentService paymentService;

    @BeforeEach
    void setUp() {
        taskExecute = new CancelOrderTaskExecute();
        orderService = mock(OrderService.class);
        cashierSupport = mock(CashierSupport.class);
        paymentService = mock(PaymentService.class);

        ReflectionTestUtils.setField(taskExecute, "orderService", orderService);
        ReflectionTestUtils.setField(taskExecute, "cashierSupport", cashierSupport);
        ReflectionTestUtils.setField(taskExecute, "paymentService", paymentService);
    }

    /**
     * 验证渠道查单已支付时触发补单，绝不执行系统取消
     */
    @Test
    @DisplayName("超时订单渠道已支付触发补单且不取消订单")
    void handleUnpaidOrderCompensatesWhenPaidOnChannel() {
        Order order = new Order();
        order.setSn("O1001");
        order.setOrderStatus(OrderStatusEnum.UNPAID.name());
        order.setPayOrderNo("pay-1001");
        order.setPaymentMethod(PaymentMethodEnum.WECHAT.name());

        when(orderService.getBySn("O1001")).thenReturn(order);

        PaymentQueryResult paidResult = PaymentQueryResult.builder()
                .exists(true)
                .isPaid(true)
                .isClosed(false)
                .paymentMethod(PaymentMethodEnum.WECHAT.name())
                .receivableNo("recv-wx-999")
                .totalAmount(88.0)
                .build();
        when(cashierSupport.query(eq(PaymentMethodEnum.WECHAT), eq("pay-1001"))).thenReturn(paidResult);

        taskExecute.handleUnpaidOrderCancel(order);

        // 验证补单被触发
        verify(paymentService).success(argThat(params ->
                "recv-wx-999".equals(params.getReceivableNo())
                        && PaymentMethodEnum.WECHAT.name().equals(params.getPaymentMethod())
                        && Double.valueOf(88.0).equals(params.getPrice())
                        && "O1001".equals(params.getPayParam().getSn())
        ));

        // 验证系统关单未执行
        verify(orderService, never()).systemCancel(anyString(), anyString(), anyBoolean());
        // 验证未调用渠道关单
        verify(cashierSupport, never()).close(any(), anyString());
    }

    /**
     * 验证渠道查单确认未支付时，先关闭渠道订单，再取消系统订单
     */
    @Test
    @DisplayName("超时订单渠道未支付先关闭渠道再取消系统订单")
    void handleUnpaidOrderClosesChannelThenCancels() {
        Order order = new Order();
        order.setSn("O1002");
        order.setOrderStatus(OrderStatusEnum.UNPAID.name());
        order.setPayOrderNo("pay-1002");
        order.setPaymentMethod(PaymentMethodEnum.ALIPAY.name());

        when(orderService.getBySn("O1002")).thenReturn(order);

        PaymentQueryResult unpaidResult = PaymentQueryResult.builder()
                .exists(true)
                .isPaid(false)
                .isClosed(false)
                .paymentMethod(PaymentMethodEnum.ALIPAY.name())
                .rawStatus("WAIT_BUYER_PAY")
                .build();
        when(cashierSupport.query(eq(PaymentMethodEnum.ALIPAY), eq("pay-1002"))).thenReturn(unpaidResult);

        taskExecute.handleUnpaidOrderCancel(order);

        // 验证渠道关单被调用
        verify(cashierSupport).close(PaymentMethodEnum.ALIPAY, "pay-1002");
        // 验证系统取消被调用
        verify(orderService).systemCancel("O1002", "超时未支付自动取消", false);
        // 验证补单未被调用
        verify(paymentService, never()).success(any());
    }

    /**
     * 验证渠道查单发生网络或网关异常时抛出异常由上层拦截，不取消订单以防误关
     */
    @Test
    @DisplayName("渠道查单异常时不取消订单以保护资金安全")
    void handleUnpaidOrderSkipsCancelOnQueryException() {
        Order order = new Order();
        order.setSn("O1003");
        order.setOrderStatus(OrderStatusEnum.UNPAID.name());
        order.setPayOrderNo("pay-1003");
        order.setPaymentMethod(PaymentMethodEnum.WECHAT.name());

        when(orderService.getBySn("O1003")).thenReturn(order);
        when(cashierSupport.query(eq(PaymentMethodEnum.WECHAT), eq("pay-1003")))
                .thenThrow(new ServiceException("微信接口网络超时"));

        assertThatThrownBy(() -> taskExecute.handleUnpaidOrderCancel(order))
                .isInstanceOf(ServiceException.class);

        // 验证系统取消未被执行
        verify(orderService, never()).systemCancel(anyString(), anyString(), anyBoolean());
        // 验证补单未被调用
        verify(paymentService, never()).success(any());
    }

    /**
     * 验证未发起过任何第三方支付请求的订单直接取消系统订单
     */
    @Test
    @DisplayName("未发起支付的超时订单直接取消系统订单")
    void handleUnpaidOrderWithoutPaymentInitiationDirectlyCancels() {
        Order order = new Order();
        order.setSn("O1004");
        order.setOrderStatus(OrderStatusEnum.UNPAID.name());
        order.setPayOrderNo(null);
        order.setPaymentMethod(null);

        when(orderService.getBySn("O1004")).thenReturn(order);

        taskExecute.handleUnpaidOrderCancel(order);

        // 验证不发起渠道查单
        verify(cashierSupport, never()).query(any(), anyString());
        // 验证直接系统取消
        verify(orderService).systemCancel("O1004", "超时未支付自动取消", false);
    }
}
