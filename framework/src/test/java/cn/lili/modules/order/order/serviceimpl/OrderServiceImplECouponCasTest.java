package cn.lili.modules.order.order.serviceimpl;

import cn.lili.common.event.TransactionCommitSendMQEvent;
import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.message.LiliMessageTemplate;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.StoreFlowService;
import cn.lili.modules.promotion.service.GiftCardCashService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.test.util.ReflectionTestUtils;

import java.math.BigDecimal;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * E_COUPON 专用完成/取消 CAS 单元测试。
 *
 * @author Mike
 * @date 2026-08-03
 */
class OrderServiceImplECouponCasTest {

    private OrderServiceImpl orderService;
    private OrderItemService orderItemService;
    private ApplicationEventPublisher applicationEventPublisher;

    @BeforeAll
    static void initMybatisEntityCache() {
        MapperBuilderAssistant assistant = new MapperBuilderAssistant(new MybatisConfiguration(), "");
        TableInfoHelper.initTableInfo(assistant, Order.class);
        TableInfoHelper.initTableInfo(assistant, OrderItem.class);
    }

    @BeforeEach
    void setUp() {
        orderService = spy(new OrderServiceImpl());
        orderItemService = mock(OrderItemService.class);
        applicationEventPublisher = mock(ApplicationEventPublisher.class);
        GiftCardCashService giftCardCashService = mock(GiftCardCashService.class);
        SettingService settingService = mock(SettingService.class);
        RocketmqCustomProperties rocketmqCustomProperties = mock(RocketmqCustomProperties.class);

        ReflectionTestUtils.setField(orderService, "orderItemService", orderItemService);
        ReflectionTestUtils.setField(orderService, "applicationEventPublisher", applicationEventPublisher);
        ReflectionTestUtils.setField(orderService, "rocketmqCustomProperties", rocketmqCustomProperties);
        ReflectionTestUtils.setField(orderService, "liliMessageTemplate", mock(LiliMessageTemplate.class));
        ReflectionTestUtils.setField(orderService, "storeFlowService", mock(StoreFlowService.class));
        ReflectionTestUtils.setField(orderService, "giftCardCashService", giftCardCashService);
        ReflectionTestUtils.setField(orderService, "settingService", settingService);

        when(rocketmqCustomProperties.getOrderTopic()).thenReturn("order-topic");
        when(giftCardCashService.resolveChannelRefundAfterGiftCard(anyString(), any(BigDecimal.class)))
                .thenReturn(BigDecimal.ZERO);
        Setting orderSetting = new Setting();
        orderSetting.setSettingValue("{\"closeComplaint\":1}");
        when(settingService.get(SettingEnum.ORDER_SETTING.name())).thenReturn(orderSetting);

        doReturn(true).when(orderItemService).update(any(LambdaUpdateWrapper.class));
    }

    /** PAID 态 E_COUPON 可 CAS 完成并触发完成副作用。 */
    @Test
    void systemCompleteECouponCompletesPaidECouponOrder() {
        Order order = paidECoupon("O001");
        doReturn(order).when(orderService).getBySn("O001");
        doReturn(true).when(orderService).update(any(LambdaUpdateWrapper.class));
        doReturn(java.util.Collections.emptyList()).when(orderItemService).getByOrderSn("O001");

        assertThat(orderService.systemCompleteECoupon("O001")).isTrue();
        verify(orderService, times(2)).update(any(LambdaUpdateWrapper.class));
        verify(applicationEventPublisher).publishEvent(any(TransactionCommitSendMQEvent.class));
    }

    /** 已完成订单幂等返回 true。 */
    @Test
    void systemCompleteECouponIsIdempotentWhenAlreadyCompleted() {
        Order order = paidECoupon("O001");
        order.setOrderStatus(OrderStatusEnum.COMPLETED.name());
        doReturn(order).when(orderService).getBySn("O001");

        assertThat(orderService.systemCompleteECoupon("O001")).isTrue();
        verify(orderService, never()).update(any(LambdaUpdateWrapper.class));
    }

    /** 已取消订单不可再完成。 */
    @Test
    void systemCompleteECouponRejectsCancelledOrder() {
        Order order = paidECoupon("O001");
        order.setOrderStatus(OrderStatusEnum.CANCELLED.name());
        doReturn(order).when(orderService).getBySn("O001");

        assertThat(orderService.systemCompleteECoupon("O001")).isFalse();
        verify(orderService, never()).update(any(LambdaUpdateWrapper.class));
    }

    /** 非 E_COUPON 订单拒绝完成 CAS。 */
    @Test
    void systemCompleteECouponRejectsVirtualOrder() {
        Order order = paidECoupon("O001");
        order.setOrderType(OrderTypeEnum.VIRTUAL.name());
        doReturn(order).when(orderService).getBySn("O001");

        assertThat(orderService.systemCompleteECoupon("O001")).isFalse();
    }

    /** PAID 态 E_COUPON 可 CAS 取消。 */
    @Test
    void systemCancelECouponCancelsPaidECouponOrder() {
        Order order = paidECoupon("O001");
        doReturn(order).when(orderService).getBySn("O001");
        doReturn(true).when(orderService).update(any(LambdaUpdateWrapper.class));

        assertThat(orderService.systemCancelECoupon("O001", "test", true)).isTrue();
        verify(orderService).update(any(LambdaUpdateWrapper.class));
    }

    /** 已取消订单幂等返回 true。 */
    @Test
    void systemCancelECouponIsIdempotentWhenAlreadyCancelled() {
        Order order = paidECoupon("O001");
        order.setOrderStatus(OrderStatusEnum.CANCELLED.name());
        doReturn(order).when(orderService).getBySn("O001");

        assertThat(orderService.systemCancelECoupon("O001", "test", true)).isTrue();
        verify(orderService, never()).update(any(LambdaUpdateWrapper.class));
    }

    /** 已完成订单不可取消。 */
    @Test
    void systemCancelECouponRejectsCompletedOrder() {
        Order order = paidECoupon("O001");
        order.setOrderStatus(OrderStatusEnum.COMPLETED.name());
        doReturn(order).when(orderService).getBySn("O001");

        assertThat(orderService.systemCancelECoupon("O001", "test", true)).isFalse();
        verify(orderService, never()).update(any(LambdaUpdateWrapper.class));
    }

    /**
     * TS-58：完成与取消 CAS 并发竞争，仅一方可写成功。
     */
    @Test
    void completeAndCancelRaceOnlyOneCasUpdateSucceeds() throws Exception {
        Order order = paidECoupon("O-RACE");
        doReturn(order).when(orderService).getBySn("O-RACE");
        doReturn(java.util.Collections.emptyList()).when(orderItemService).getByOrderSn("O-RACE");

        AtomicInteger casUpdates = new AtomicInteger(0);
        doAnswer(invocation -> {
            if (casUpdates.incrementAndGet() == 1) {
                return true;
            }
            return false;
        }).when(orderService).update(any(LambdaUpdateWrapper.class));

        CountDownLatch start = new CountDownLatch(1);
        ExecutorService pool = Executors.newFixedThreadPool(2);
        try {
            Future<Boolean> completeFuture = pool.submit(() -> {
                start.await(5, TimeUnit.SECONDS);
                return orderService.systemCompleteECoupon("O-RACE");
            });
            Future<Boolean> cancelFuture = pool.submit(() -> {
                start.await(5, TimeUnit.SECONDS);
                return orderService.systemCancelECoupon("O-RACE", "user cancel", true);
            });
            start.countDown();

            boolean completeOk = completeFuture.get(10, TimeUnit.SECONDS);
            boolean cancelOk = cancelFuture.get(10, TimeUnit.SECONDS);

            assertThat(completeOk ^ cancelOk).isTrue();
            assertThat(casUpdates.get()).isEqualTo(2);
            verify(orderService, times(2)).update(any(LambdaUpdateWrapper.class));
        } finally {
            pool.shutdownNow();
        }
    }

    private Order paidECoupon(String sn) {
        Order order = new Order();
        order.setSn(sn);
        order.setOrderType(OrderTypeEnum.E_COUPON.name());
        order.setOrderStatus(OrderStatusEnum.PAID.name());
        order.setPayStatus(PayStatusEnum.PAID.name());
        order.setMemberId("m1");
        order.setFlowPrice(10D);
        order.setPaymentMethod("WALLET");
        return order;
    }
}
