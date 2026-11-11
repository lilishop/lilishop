package cn.lili.modules.payment.kit;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.Trade;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.order.serviceimpl.OrderServiceImpl;
import cn.lili.modules.order.order.serviceimpl.TradeServiceImpl;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.modules.wallet.entity.dos.Recharge;
import cn.lili.modules.wallet.service.MemberWalletService;
import cn.lili.modules.wallet.serviceimpl.RechargeServiceImpl;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

/**
 * 支付结果通知商户号与金额核验单元测试
 *
 * @author Bulbasaur
 * @since 2026-11-11
 */
class PaymentNotificationValidationTest {

    /**
     * 普通订单入账时，实付金额与订单应付金额不一致必须拒绝并抛出 PAY_INCONSISTENT_ERROR
     */
    @Test
    @DisplayName("普通订单入账金额不一致拦截测试")
    void payOrderRejectsMismatchedAmount() {
        OrderServiceImpl orderService = spy(new OrderServiceImpl());
        Order order = new Order();
        order.setSn("O1001");
        order.setFlowPrice(100.0);
        order.setOrderStatus(OrderStatusEnum.UNPAID.name());
        order.setPayStatus(PayStatusEnum.UNPAID.name());

        doReturn(order).when(orderService).getBySn("O1001");

        // 尝试以 0.01 元完成 100 元订单入账
        assertThatThrownBy(() -> orderService.payOrder("O1001", PaymentMethodEnum.WECHAT.name(), "wx_recv_1", 0.01))
                .isInstanceOf(ServiceException.class)
                .hasMessage(ResultCode.PAY_INCONSISTENT_ERROR.message());

        // 验证订单状态未被修改为 PAID
        assertThat(order.getPayStatus()).isEqualTo(PayStatusEnum.UNPAID.name());
    }

    /**
     * 整笔交易入账时，实付金额与交易总额不一致必须拒绝并抛出 PAY_INCONSISTENT_ERROR
     */
    @Test
    @DisplayName("整笔交易入账金额不一致拦截测试")
    void payTradeRejectsMismatchedAmount() {
        TradeServiceImpl tradeService = spy(new TradeServiceImpl());
        OrderService orderService = mock(OrderService.class);
        ReflectionTestUtils.setField(tradeService, "orderService", orderService);

        Trade trade = new Trade();
        trade.setSn("T1001");
        trade.setFlowPrice(299.0);
        trade.setPayStatus(PayStatusEnum.UNPAID.name());

        doReturn(trade).when(tradeService).getBySn("T1001");

        // 尝试以 1.00 元完成 299 元交易入账
        assertThatThrownBy(() -> tradeService.payTrade("T1001", PaymentMethodEnum.ALIPAY.name(), "ali_recv_1", 1.0))
                .isInstanceOf(ServiceException.class)
                .hasMessage(ResultCode.PAY_INCONSISTENT_ERROR.message());

        verify(orderService, never()).payOrder(any(), any(), any(), any());
    }

    /**
     * 客户余额充值入账时，实付金额与充值单金额不一致必须拒绝并抛出 PAY_INCONSISTENT_ERROR
     */
    @Test
    @DisplayName("余额充值入账金额不一致拦截测试")
    void rechargePaySuccessRejectsMismatchedAmount() {
        RechargeServiceImpl rechargeService = spy(new RechargeServiceImpl());
        MemberWalletService memberWalletService = mock(MemberWalletService.class);
        ReflectionTestUtils.setField(rechargeService, "memberWalletService", memberWalletService);

        Recharge recharge = new Recharge();
        recharge.setRechargeSn("RC1001");
        recharge.setRechargeMoney(500.0);
        recharge.setPayStatus(PayStatusEnum.UNPAID.name());

        doReturn(recharge).when(rechargeService).getOne(any(QueryWrapper.class));

        // 尝试以 0.01 元充值 500 元
        assertThatThrownBy(() -> rechargeService.paySuccess("RC1001", "recv_1", PaymentMethodEnum.WECHAT.name(), 0.01))
                .isInstanceOf(ServiceException.class)
                .hasMessage(ResultCode.PAY_INCONSISTENT_ERROR.message());

        verify(memberWalletService, never()).increase(any());
    }
}
