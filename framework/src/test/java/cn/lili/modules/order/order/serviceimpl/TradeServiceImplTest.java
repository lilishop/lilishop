package cn.lili.modules.order.order.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.Trade;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.service.OrderService;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

/**
 * 交易服务支付协调回归测试，覆盖金额核验、分订单入账和失败传播，不连接数据库或支付渠道。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
class TradeServiceImplTest {
    private final OrderService orders = mock(OrderService.class);
    private final TradeServiceImpl service = spy(new TradeServiceImpl());
    private final Trade trade = new Trade();

    /** 准备应付100元的交易及订单服务依赖，交易保存使用模拟对象验证调用。 */
    @BeforeEach
    void setUp() {
        trade.setSn("trade");
        trade.setFlowPrice(100D);
        trade.setPayStatus(PayStatusEnum.UNPAID.name());
        ReflectionTestUtils.setField(service, "orderService", orders);
        doReturn(trade).when(service).getBySn("trade");
        doReturn(true).when(service).saveOrUpdate(any(Trade.class));
    }

    /** 验证交易不存在时拒绝支付，禁止继续查询或更新订单。 */
    @Test
    void missingTradeDoesNotAttemptOrderPayment() {
        doReturn(null).when(service).getBySn("trade");
        assertThatThrownBy(() -> service.payTrade("trade", "WECHAT", "receipt", 100D))
                .isInstanceOf(ServiceException.class);
        verifyNoInteractions(orders);
        verify(service, never()).saveOrUpdate(any(Trade.class));
    }

    /** 验证金额按两位小数四舍五入后不一致时拒绝入账，不修改交易支付状态。 */
    @Test
    void inconsistentRoundedAmountDoesNotWritePayment() {
        assertThatThrownBy(() -> service.payTrade("trade", "WECHAT", "receipt", 100.005D))
                .isInstanceOfSatisfying(ServiceException.class,
                        error -> assertThat(error.getResultCode()).isEqualTo(ResultCode.PAY_INCONSISTENT_ERROR));
        assertThat(trade.getPayStatus()).isEqualTo(PayStatusEnum.UNPAID.name());
        verifyNoInteractions(orders);
        verify(service, never()).saveOrUpdate(any(Trade.class));
    }

    /** 验证通知金额核验通过后，按每个子订单自身应付金额入账，最后标记交易已支付。 */
    @Test
    void paysOrdersWithTheirOwnAmountsBeforeSavingTrade() {
        Order first = new Order();
        first.setSn("first");
        first.setFlowPrice(30D);
        Order second = new Order();
        second.setSn("second");
        second.setFlowPrice(70D);
        when(orders.list(org.mockito.ArgumentMatchers.<Wrapper<Order>>any())).thenReturn(List.of(first, second));

        service.payTrade("trade", "WECHAT", "receipt", 100.004D);

        var sequence = inOrder(orders, service);
        sequence.verify(orders).payOrder("first", "WECHAT", "receipt", 30D);
        sequence.verify(orders).payOrder("second", "WECHAT", "receipt", 70D);
        sequence.verify(service).saveOrUpdate(trade);
        assertThat(trade.getPayStatus()).isEqualTo(PayStatusEnum.PAID.name());
    }

    /** 验证子订单支付异常继续向上传播，供外层交易事务回滚，禁止标记交易已支付。 */
    @Test
    void orderPaymentFailurePropagatesWithoutSavingPaidTrade() {
        Order order = new Order();
        order.setSn("order");
        order.setFlowPrice(100D);
        when(orders.list(org.mockito.ArgumentMatchers.<Wrapper<Order>>any())).thenReturn(List.of(order));
        RuntimeException failure = new IllegalStateException("订单支付失败");
        doThrow(failure).when(orders).payOrder("order", "WECHAT", "receipt", 100D);

        assertThatThrownBy(() -> service.payTrade("trade", "WECHAT", "receipt", 100D)).isSameAs(failure);
        assertThat(trade.getPayStatus()).isEqualTo(PayStatusEnum.UNPAID.name());
        verify(service, never()).saveOrUpdate(any(Trade.class));
    }
}
