package cn.lili.modules.order.order.serviceimpl;

import cn.lili.common.event.TransactionCommitSendMQEvent;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.modules.distribution.event.DistributionPaymentRewardsEvent;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.service.StoreFlowService;
import cn.lili.modules.order.trade.entity.dos.OrderLog;
import cn.lili.modules.order.trade.service.OrderLogService;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.rocketmq.tags.OrderTagsEnum;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.context.ApplicationEvent;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * payOrder 只发布分销入账事件，支付事务内不再同步写分销。
 *
 * @author Mike
 * @date 2026-09-07
 */
class OrderServiceImplPayOrderTest {

    private OrderServiceImpl orderService;
    private StoreFlowService storeFlowService;
    private OrderLogService orderLogService;
    private ApplicationEventPublisher applicationEventPublisher;

    @BeforeAll
    static void initMybatisEntityCache() {
        MapperBuilderAssistant assistant = new MapperBuilderAssistant(new MybatisConfiguration(), "");
        TableInfoHelper.initTableInfo(assistant, Order.class);
    }

    @BeforeEach
    void setUp() {
        orderService = spy(new OrderServiceImpl());
        storeFlowService = mock(StoreFlowService.class);
        orderLogService = mock(OrderLogService.class);
        applicationEventPublisher = mock(ApplicationEventPublisher.class);
        RocketmqCustomProperties rocketmqCustomProperties = mock(RocketmqCustomProperties.class);

        ReflectionTestUtils.setField(orderService, "storeFlowService", storeFlowService);
        ReflectionTestUtils.setField(orderService, "orderLogService", orderLogService);
        ReflectionTestUtils.setField(orderService, "applicationEventPublisher", applicationEventPublisher);
        ReflectionTestUtils.setField(orderService, "rocketmqCustomProperties", rocketmqCustomProperties);

        when(rocketmqCustomProperties.getOrderTopic()).thenReturn("order-topic");
        when(orderLogService.save(any(OrderLog.class))).thenReturn(true);
        doReturn(true).when(orderService).updateById(any(Order.class));
    }

    /**
     * 支付成功发布分销入账事件，订单保持已支付。
     */
    @Test
    void payOrderPublishesDistributionEventAndKeepsOrderPaid() {
        Order order = unpaidOrder("O001");
        doReturn(order).when(orderService).getBySn("O001");

        // 支付成功：只发布分销入账事件，事务内不写分销
        orderService.payOrder("O001", PaymentMethodEnum.WECHAT.name(), "recv-1");

        assertThat(order.getPayStatus()).isEqualTo(PayStatusEnum.PAID.name());
        assertThat(order.getOrderStatus()).isEqualTo(OrderStatusEnum.PAID.name());
        verify(storeFlowService).payOrder("O001");
        verify(orderLogService).save(any(OrderLog.class));

        ArgumentCaptor<ApplicationEvent> eventCaptor = ArgumentCaptor.forClass(ApplicationEvent.class);
        verify(applicationEventPublisher, times(2)).publishEvent(eventCaptor.capture());
        List<ApplicationEvent> events = eventCaptor.getAllValues();
        assertThat(events.get(0)).isInstanceOf(TransactionCommitSendMQEvent.class);
        TransactionCommitSendMQEvent mqEvent = (TransactionCommitSendMQEvent) events.get(0);
        assertThat(mqEvent.getTag()).isEqualTo(OrderTagsEnum.STATUS_CHANGE.name());
        assertThat(events.get(1)).isInstanceOf(DistributionPaymentRewardsEvent.class);
        assertThat(((DistributionPaymentRewardsEvent) events.get(1)).getOrderSn()).isEqualTo("O001");
    }

    /**
     * 已支付订单不再发布分销事件。
     */
    @Test
    void alreadyPaidOrderDoesNotPublishDistributionEvent() {
        Order order = unpaidOrder("O001");
        order.setPayStatus(PayStatusEnum.PAID.name());
        doReturn(order).when(orderService).getBySn("O001");

        // 已支付订单拒绝再次支付，且不发分销事件
        assertThatThrownBy(() -> orderService.payOrder("O001", PaymentMethodEnum.WECHAT.name(), "recv-1"))
                .isInstanceOf(ServiceException.class);

        verify(applicationEventPublisher, never()).publishEvent(any());
        verify(storeFlowService, never()).payOrder(anyString());
    }

    /**
     * 构造未支付订单。
     */
    private static Order unpaidOrder(String sn) {
        Order order = new Order();
        order.setSn(sn);
        order.setPayStatus(PayStatusEnum.UNPAID.name());
        order.setOrderStatus(OrderStatusEnum.UNPAID.name());
        return order;
    }
}
