package cn.lili.event.impl;

import cn.lili.cache.Cache;
import cn.lili.modules.goods.service.CardKeyService;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.entity.vo.OrderDetailVO;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.service.KanjiaActivityGoodsService;
import cn.lili.modules.promotion.service.KanjiaActivityService;
import cn.lili.modules.promotion.service.PointsGoodsService;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.core.ValueOperations;
import org.springframework.data.redis.core.script.DefaultRedisScript;
import org.springframework.test.util.ReflectionTestUtils;

import java.time.Duration;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * StockUpdateExecute PAID 路由：E_COUPON 仅扣促销库存后 afterOrderConfirm。
 *
 * @author Mike
 * @date 2026-08-02
 */
class StockUpdateExecuteRoutingTest {

    private StockUpdateExecute stockUpdateExecute;
    private OrderService orderService;
    private CardKeyService cardKeyService;
    private Cache cache;
    private StringRedisTemplate stringRedisTemplate;
    private ValueOperations<String, String> valueOperations;

    private PromotionGoodsService promotionGoodsService;

    @BeforeEach
    void setUp() {
        stockUpdateExecute = new StockUpdateExecute();
        orderService = mock(OrderService.class);
        cardKeyService = mock(CardKeyService.class);
        cache = mock(Cache.class);
        stringRedisTemplate = mock(StringRedisTemplate.class);
        valueOperations = mock(ValueOperations.class);
        promotionGoodsService = mock(PromotionGoodsService.class);
        DefaultRedisScript<Boolean> quantityScript = mock(DefaultRedisScript.class);

        ReflectionTestUtils.setField(stockUpdateExecute, "orderService", orderService);
        ReflectionTestUtils.setField(stockUpdateExecute, "cardKeyService", cardKeyService);
        ReflectionTestUtils.setField(stockUpdateExecute, "cache", cache);
        ReflectionTestUtils.setField(stockUpdateExecute, "stringRedisTemplate", stringRedisTemplate);
        ReflectionTestUtils.setField(stockUpdateExecute, "quantityScript", quantityScript);
        ReflectionTestUtils.setField(stockUpdateExecute, "goodsSkuService", mock(GoodsSkuService.class));
        ReflectionTestUtils.setField(stockUpdateExecute, "promotionGoodsService", promotionGoodsService);
        ReflectionTestUtils.setField(stockUpdateExecute, "kanjiaActivityService", mock(KanjiaActivityService.class));
        ReflectionTestUtils.setField(stockUpdateExecute, "kanjiaActivityGoodsService", mock(KanjiaActivityGoodsService.class));
        ReflectionTestUtils.setField(stockUpdateExecute, "pointsGoodsService", mock(PointsGoodsService.class));

        when(stringRedisTemplate.opsForValue()).thenReturn(valueOperations);
    }

    private void mockPaidLockAcquired() {
        when(valueOperations.setIfAbsent(anyString(), anyString(), any(Duration.class))).thenReturn(true);
        when(valueOperations.get(anyString())).thenReturn("token");
    }

    private void mockPaidLockBusy() {
        when(valueOperations.setIfAbsent(anyString(), anyString(), any(Duration.class))).thenReturn(false);
    }

    @Test
    void paidMessageRoutesECouponToAfterOrderConfirmWithoutSkuLua() {
        mockPaidLockAcquired();
        String orderSn = "O-ECOUPON-001";
        Order order = new Order();
        order.setSn(orderSn);
        order.setOrderType(OrderTypeEnum.E_COUPON.name());

        OrderItem orderItem = new OrderItem();
        orderItem.setSkuId("sku-1");
        orderItem.setNum(1);

        OrderDetailVO detail = new OrderDetailVO();
        detail.setOrder(order);
        detail.setOrderItems(List.of(orderItem));

        when(orderService.queryDetail(orderSn)).thenReturn(detail);
        when(cardKeyService.isECouponFulfillmentEligible(order)).thenReturn(true);

        OrderMessage message = new OrderMessage();
        message.setOrderSn(orderSn);
        message.setNewStatus(OrderStatusEnum.PAID);

        stockUpdateExecute.orderChange(message);

        verify(orderService).afterOrderConfirm(orderSn);
        verify(cardKeyService, never()).fulfillAfterPayment(orderSn);
        verify(stringRedisTemplate, never()).execute(any(DefaultRedisScript.class), anyList(), any());
    }

    @Test
    void cancelledPaidECouponReleasesCardsAndSkipsSkuLua() {
        String orderSn = "O-ECOUPON-CANCEL";
        Order order = new Order();
        order.setSn(orderSn);
        order.setOrderType(OrderTypeEnum.E_COUPON.name());
        order.setPayStatus(PayStatusEnum.PAID.name());
        order.setCancelReason("买家取消");

        OrderDetailVO detail = new OrderDetailVO();
        detail.setOrder(order);
        detail.setOrderItems(List.of(new OrderItem()));

        when(orderService.queryDetail(orderSn)).thenReturn(detail);

        OrderMessage message = new OrderMessage();
        message.setOrderSn(orderSn);
        message.setNewStatus(OrderStatusEnum.CANCELLED);

        stockUpdateExecute.orderChange(message);

        verify(cardKeyService).releaseReservation(orderSn);
        verify(cardKeyService).voidOnSystemFailure(orderSn);
        verify(stringRedisTemplate, never()).execute(any(DefaultRedisScript.class), anyList(), any());
    }

    @Test
    void paidMessageSkipsWhenLockNotAcquired() {
        mockPaidLockBusy();
        String orderSn = "O-ECOUPON-LOCK";
        OrderMessage message = new OrderMessage();
        message.setOrderSn(orderSn);
        message.setNewStatus(OrderStatusEnum.PAID);

        stockUpdateExecute.orderChange(message);

        verify(orderService, never()).queryDetail(orderSn);
        verify(orderService, never()).afterOrderConfirm(any());
    }

    @Test
    void paidMessageSkipsWhenECouponAlreadyCompleted() {
        mockPaidLockAcquired();
        String orderSn = "O-ECOUPON-DONE";
        Order order = new Order();
        order.setSn(orderSn);
        order.setOrderType(OrderTypeEnum.E_COUPON.name());
        order.setOrderStatus(OrderStatusEnum.COMPLETED.name());

        OrderDetailVO detail = new OrderDetailVO();
        detail.setOrder(order);
        detail.setOrderItems(List.of(new OrderItem()));
        when(orderService.queryDetail(orderSn)).thenReturn(detail);

        OrderMessage message = new OrderMessage();
        message.setOrderSn(orderSn);
        message.setNewStatus(OrderStatusEnum.PAID);

        stockUpdateExecute.orderChange(message);

        verify(orderService, never()).afterOrderConfirm(orderSn);
    }

    @Test
    void paidMessageRoutesECouponWithPromotionStockDeduction() {
        mockPaidLockAcquired();
        String orderSn = "O-ECOUPON-PROMO";
        Order order = new Order();
        order.setSn(orderSn);
        order.setOrderType(OrderTypeEnum.E_COUPON.name());
        order.setGoodsNum(1);

        OrderItem orderItem = new OrderItem();
        orderItem.setSkuId("sku-1");
        orderItem.setNum(1);
        orderItem.setPromotionType("PINTUAN");
        orderItem.setPromotionId("act-1");

        OrderDetailVO detail = new OrderDetailVO();
        detail.setOrder(order);
        detail.setOrderItems(List.of(orderItem));

        when(orderService.queryDetail(orderSn)).thenReturn(detail);
        when(cache.hasKey(anyString())).thenReturn(true);
        when(cache.get(anyString())).thenReturn("10");
        when(cache.multiGet(anyList())).thenReturn(List.of(10));
        when(promotionGoodsService.getPromotionsGoods(any())).thenReturn(new PromotionGoods());
        when(stringRedisTemplate.execute(any(DefaultRedisScript.class), anyList(), any())).thenReturn(true);
        when(cardKeyService.isECouponFulfillmentEligible(order)).thenReturn(true);

        OrderMessage message = new OrderMessage();
        message.setOrderSn(orderSn);
        message.setNewStatus(OrderStatusEnum.PAID);

        stockUpdateExecute.orderChange(message);

        verify(stringRedisTemplate).execute(any(DefaultRedisScript.class), anyList(), any());
        verify(orderService).afterOrderConfirm(orderSn);
    }

    @Test
    void paidMessageSkipsCancelledECouponBeforeFulfillment() {
        mockPaidLockAcquired();
        String orderSn = "O-ECOUPON-CANCELLED";
        Order order = new Order();
        order.setSn(orderSn);
        order.setOrderType(OrderTypeEnum.E_COUPON.name());
        order.setOrderStatus(OrderStatusEnum.CANCELLED.name());
        order.setPayStatus(PayStatusEnum.PAID.name());

        OrderDetailVO detail = new OrderDetailVO();
        detail.setOrder(order);
        detail.setOrderItems(List.of(new OrderItem()));
        when(orderService.queryDetail(orderSn)).thenReturn(detail);
        when(cardKeyService.isECouponFulfillmentEligible(order)).thenReturn(false);

        OrderMessage message = new OrderMessage();
        message.setOrderSn(orderSn);
        message.setNewStatus(OrderStatusEnum.PAID);

        stockUpdateExecute.orderChange(message);

        verify(orderService, never()).afterOrderConfirm(orderSn);
        verify(stringRedisTemplate, never()).execute(any(DefaultRedisScript.class), anyList(), any());
    }

    @Test
    void paidMessageSkipsUnpaidECouponBeforeFulfillment() {
        mockPaidLockAcquired();
        String orderSn = "O-ECOUPON-UNPAID";
        Order order = new Order();
        order.setSn(orderSn);
        order.setOrderType(OrderTypeEnum.E_COUPON.name());
        order.setOrderStatus(OrderStatusEnum.UNPAID.name());
        order.setPayStatus(PayStatusEnum.UNPAID.name());

        OrderDetailVO detail = new OrderDetailVO();
        detail.setOrder(order);
        detail.setOrderItems(List.of(new OrderItem()));
        when(orderService.queryDetail(orderSn)).thenReturn(detail);
        when(cardKeyService.isECouponFulfillmentEligible(order)).thenReturn(false);

        OrderMessage message = new OrderMessage();
        message.setOrderSn(orderSn);
        message.setNewStatus(OrderStatusEnum.PAID);

        stockUpdateExecute.orderChange(message);

        verify(orderService, never()).afterOrderConfirm(orderSn);
    }

    @Test
    void paidMessageRoutesVirtualOrderToAfterOrderConfirmWithSkuLua() {
        mockPaidLockAcquired();
        String orderSn = "O-VIRTUAL-001";
        Order order = new Order();
        order.setSn(orderSn);
        order.setOrderType(OrderTypeEnum.VIRTUAL.name());

        OrderItem orderItem = new OrderItem();
        orderItem.setSkuId("sku-2");
        orderItem.setNum(1);

        OrderDetailVO detail = new OrderDetailVO();
        detail.setOrder(order);
        detail.setOrderItems(List.of(orderItem));

        when(orderService.queryDetail(orderSn)).thenReturn(detail);
        when(orderService.getBySn(orderSn)).thenReturn(order);
        when(cache.multiGet(anyList())).thenReturn(List.of(10));
        when(stringRedisTemplate.execute(any(DefaultRedisScript.class), anyList(), any())).thenReturn(true);

        OrderMessage message = new OrderMessage();
        message.setOrderSn(orderSn);
        message.setNewStatus(OrderStatusEnum.PAID);

        stockUpdateExecute.orderChange(message);

        verify(orderService).afterOrderConfirm(orderSn);
        verify(cardKeyService, never()).fulfillAfterPayment(any());
    }
}

