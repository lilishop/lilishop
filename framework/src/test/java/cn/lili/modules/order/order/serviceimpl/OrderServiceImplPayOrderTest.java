package cn.lili.modules.order.order.serviceimpl;

import cn.lili.common.event.TransactionCommitSendMQEvent;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.modules.distribution.event.DistributionPaymentRewardsEvent;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.StoreFlowService;
import cn.lili.modules.promotion.service.GiftCardCashService;
import java.math.BigDecimal;
import static org.mockito.ArgumentMatchers.eq;
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

    /**
     * 初始化当前测试使用的MyBatis-Plus实体元数据。
     */
    @BeforeAll
    static void initMybatisEntityCache() {
        MapperBuilderAssistant assistant = new MapperBuilderAssistant(new MybatisConfiguration(), "");
        TableInfoHelper.initTableInfo(assistant, Order.class);
        TableInfoHelper.initTableInfo(assistant, OrderItem.class);
    }

    /**
     * 初始化当前用例的模拟依赖及测试数据。
     */
    @BeforeEach
    void setUp() {
        orderService = spy(new OrderServiceImpl());
        ReflectionTestUtils.setField(orderService, "pickupCardOrderService", org.mockito.Mockito.mock(cn.lili.modules.promotion.service.PickupCardOrderService.class));
        storeFlowService = mock(StoreFlowService.class);
        orderLogService = mock(OrderLogService.class);
        applicationEventPublisher = mock(ApplicationEventPublisher.class);
        RocketmqCustomProperties rocketmqCustomProperties = mock(RocketmqCustomProperties.class);

        ReflectionTestUtils.setField(orderService, "storeFlowService", storeFlowService);
        ReflectionTestUtils.setField(orderService, "orderLogService", orderLogService);
        ReflectionTestUtils.setField(orderService, "applicationEventPublisher", applicationEventPublisher);
        ReflectionTestUtils.setField(orderService, "rocketmqCustomProperties", rocketmqCustomProperties);

        OrderItemService orderItemService = mock(OrderItemService.class);
        GiftCardCashService giftCardCashService = mock(GiftCardCashService.class);
        ReflectionTestUtils.setField(orderService, "orderItemService", orderItemService);
        ReflectionTestUtils.setField(orderService, "giftCardCashService", giftCardCashService);
        when(rocketmqCustomProperties.getOrderTopic()).thenReturn("order-topic");
        when(orderLogService.save(any(OrderLog.class))).thenReturn(true);
        doReturn(true).when(orderService).updateById(any(Order.class));
        doReturn(true).when(orderService).update(any());
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
    /**
     * 已取消订单收到延迟支付时拒绝入账复活，不产生店铺流水与分销，记录日志。
     */
    @Test
    void payOrderRefusesCancelledOrderAndDoesNotReviveIt() {
        Order order = unpaidOrder("O002");
        order.setOrderStatus(OrderStatusEnum.CANCELLED.name());
        order.setFlowPrice(100.0);
        order.setMemberId("M001");
        doReturn(order).when(orderService).getBySn("O002");

        orderService.payOrder("O002", PaymentMethodEnum.WECHAT.name(), "recv-late-1");

        // 订单状态绝不被复活为 PAID
        assertThat(order.getOrderStatus()).isEqualTo(OrderStatusEnum.CANCELLED.name());
        assertThat(order.getPayStatus()).isEqualTo(PayStatusEnum.UNPAID.name());
        // 不记录店铺支付流水
        verify(storeFlowService, never()).payOrder("O002");
        // 不发布入账与分销事件
        verify(applicationEventPublisher, never()).publishEvent(any());
        // 记录系统退款操作日志
        verify(orderLogService).save(any(OrderLog.class));
    }

    /**
     * 超时关单（无需退款）仅允许针对 UNPAID 订单，若已是 PAID 状态则拒绝关单。
     */
    @Test
    void systemCancelRejectsAlreadyPaidOrderWhenRefundMoneyIsFalse() {
        Order order = unpaidOrder("O003");
        order.setOrderStatus(OrderStatusEnum.PAID.name());
        order.setPayStatus(PayStatusEnum.PAID.name());
        doReturn(order).when(orderService).getBySn("O003");

        // 尝试超时关单
        orderService.systemCancel("O003", "超时未支付自动取消", false);

        // 验证订单状态未被变更为 CANCELLED
        assertThat(order.getOrderStatus()).isEqualTo(OrderStatusEnum.PAID.name());
        // 验证未调用任何取消更新
        verify(orderService, never()).update(any());
    }

    /**
     * 超时关单（未付款，refundMoney=false）必须执行礼品卡回卡并发送订单取消事件。
     */
    @Test
    void systemCancelUnpaidOrderRecoversGiftCardAndPublishesCancelEvent() {
        Order order = unpaidOrder("O004");
        order.setPayStatus(PayStatusEnum.UNPAID.name());
        order.setOrderStatus(OrderStatusEnum.UNPAID.name());
        doReturn(order).when(orderService).getBySn("O004");

        orderService.systemCancel("O004", "超时未支付自动取消", false);

        // 验证礼品卡回卡被调用（orderAmount为0表示未付款回卡）
        GiftCardCashService giftCardCashService = (GiftCardCashService) ReflectionTestUtils.getField(orderService, "giftCardCashService");
        verify(giftCardCashService).resolveChannelRefundAfterGiftCard(eq("O004"), eq(BigDecimal.ZERO));

        // 验证状态变更为已取消
        assertThat(order.getOrderStatus()).isEqualTo(OrderStatusEnum.CANCELLED.name());

        // 验证取消事件被广播发布（驱动优惠券与积分恢复）
        ArgumentCaptor<ApplicationEvent> captor = ArgumentCaptor.forClass(ApplicationEvent.class);
        verify(applicationEventPublisher).publishEvent(captor.capture());
        ApplicationEvent event = captor.getValue();
        assertThat(event).isInstanceOf(TransactionCommitSendMQEvent.class);
        TransactionCommitSendMQEvent mqEvent = (TransactionCommitSendMQEvent) event;
        assertThat(mqEvent.getTag()).isEqualTo(OrderTagsEnum.STATUS_CHANGE.name());
    }

    private static Order unpaidOrder(String sn) {
        Order order = new Order();
        order.setSn(sn);
        order.setPayStatus(PayStatusEnum.UNPAID.name());
        order.setOrderStatus(OrderStatusEnum.UNPAID.name());
        return order;
    }
}
