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
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
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
    private GoodsSkuService goodsSkuService;
    private DefaultRedisScript<Boolean> quantityScript;

    /**
     * 初始化当前用例的模拟依赖及测试数据。
     */
    @BeforeEach
    void setUp() {
        stockUpdateExecute = new StockUpdateExecute();
        orderService = mock(OrderService.class);
        cardKeyService = mock(CardKeyService.class);
        cache = mock(Cache.class);
        stringRedisTemplate = mock(StringRedisTemplate.class);
        valueOperations = mock(ValueOperations.class);
        promotionGoodsService = mock(PromotionGoodsService.class);
        quantityScript = mock(DefaultRedisScript.class);

        ReflectionTestUtils.setField(stockUpdateExecute, "orderService", orderService);
        ReflectionTestUtils.setField(stockUpdateExecute, "cardKeyService", cardKeyService);
        ReflectionTestUtils.setField(stockUpdateExecute, "cache", cache);
        ReflectionTestUtils.setField(stockUpdateExecute, "stringRedisTemplate", stringRedisTemplate);
        goodsSkuService = mock(GoodsSkuService.class);
        ReflectionTestUtils.setField(stockUpdateExecute, "quantityScript", quantityScript);
        ReflectionTestUtils.setField(stockUpdateExecute, "goodsSkuService", goodsSkuService);
        ReflectionTestUtils.setField(stockUpdateExecute, "promotionGoodsService", promotionGoodsService);
        ReflectionTestUtils.setField(stockUpdateExecute, "kanjiaActivityService", mock(KanjiaActivityService.class));
        ReflectionTestUtils.setField(stockUpdateExecute, "kanjiaActivityGoodsService", mock(KanjiaActivityGoodsService.class));
        ReflectionTestUtils.setField(stockUpdateExecute, "pointsGoodsService", mock(PointsGoodsService.class));

        when(stringRedisTemplate.opsForValue()).thenReturn(valueOperations);
    }

    /**
     * 模拟成功获取订单付款处理锁。
     */
    private void mockPaidLockAcquired() {
        when(valueOperations.setIfAbsent(anyString(), anyString(), any(Duration.class))).thenReturn(true);
        when(valueOperations.get(anyString())).thenReturn("token");
    }

    /**
     * 模拟订单付款处理锁已被其他流程占用。
     */
    private void mockPaidLockBusy() {
        when(valueOperations.setIfAbsent(anyString(), anyString(), any(Duration.class))).thenReturn(false);
    }

    /**
     * 验证卡密付款消息路由到订单确认且不执行SKU库存Lua。
     */
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
        verify(stringRedisTemplate, never()).execute(eq(quantityScript), anyList(), any());
    }

    /**
     * 验证已付款卡密取消时释放卡密且不执行SKU库存Lua。
     */
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
        verify(stringRedisTemplate, never()).execute(eq(quantityScript), anyList(), any());
    }

    /**
     * 验证付款消息未获取处理锁时跳过执行。
     */
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

    /**
     * 验证已完成卡密订单的付款消息不重复处理。
     */
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

    /**
     * 验证卡密付款消息同时处理促销库存扣减。
     */
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
        when(stringRedisTemplate.execute(eq(quantityScript), anyList(), any())).thenReturn(true);
        when(cardKeyService.isECouponFulfillmentEligible(order)).thenReturn(true);

        OrderMessage message = new OrderMessage();
        message.setOrderSn(orderSn);
        message.setNewStatus(OrderStatusEnum.PAID);

        stockUpdateExecute.orderChange(message);

        verify(stringRedisTemplate).execute(eq(quantityScript), anyList(), any());
        verify(orderService).afterOrderConfirm(orderSn);
    }

    /**
     * 验证已取消卡密订单不进入付款履约。
     */
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
        verify(stringRedisTemplate, never()).execute(eq(quantityScript), anyList(), any());
    }

    /**
     * 验证尚未付款卡密订单不进入付款履约。
     */
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

    /**
     * 验证虚拟订单付款后扣减SKU库存并进入订单确认。
     */
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
        when(stringRedisTemplate.execute(eq(quantityScript), anyList(), any())).thenReturn(true);

        OrderMessage message = new OrderMessage();
        message.setOrderSn(orderSn);
        message.setNewStatus(OrderStatusEnum.PAID);

        stockUpdateExecute.orderChange(message);

        verify(orderService).afterOrderConfirm(orderSn);
        verify(cardKeyService, never()).fulfillAfterPayment(any());
    }

    /**
     * 验证任一SKU库存缓存缺失时先初始化缓存。
     */
    @Test
    void paidMessageInitializesCacheWhenAnySkuCacheIsMissing() {
        mockPaidLockAcquired();
        String orderSn = "O-MULTI-SKU-001";
        Order order = new Order();
        order.setSn(orderSn);
        order.setOrderType(OrderTypeEnum.NORMAL.name());

        OrderItem item1 = new OrderItem();
        item1.setSkuId("sku-A");
        item1.setNum(1);

        OrderItem item2 = new OrderItem();
        item2.setSkuId("sku-B");
        item2.setNum(2);

        OrderDetailVO detail = new OrderDetailVO();
        detail.setOrder(order);
        detail.setOrderItems(List.of(item1, item2));

        when(orderService.queryDetail(orderSn)).thenReturn(detail);
        when(orderService.getBySn(orderSn)).thenReturn(order);
        // sku-A 缓存命中 10，sku-B 缓存失效为 null
        java.util.List<Integer> mockStocks = new java.util.ArrayList<>();
        mockStocks.add(10);
        mockStocks.add(null);
        when(cache.multiGet(anyList())).thenReturn(mockStocks);
        when(cache.hasKey(GoodsSkuService.getStockCacheKey("sku-A"))).thenReturn(true);
        when(cache.hasKey(GoodsSkuService.getStockCacheKey("sku-B"))).thenReturn(false);
        when(stringRedisTemplate.execute(eq(quantityScript), anyList(), any(), any())).thenReturn(true);

        OrderMessage message = new OrderMessage();
        message.setOrderSn(orderSn);
        message.setNewStatus(OrderStatusEnum.PAID);

        stockUpdateExecute.orderChange(message);

        // 验证 sku-B 缺失时，必须触发 goodsSkuService.getStock 回填缓存
        verify(goodsSkuService).getStock("sku-B");
        verify(orderService).afterOrderConfirm(orderSn);
    }

    /**
     * 验证付款消息处理完成后使用解锁脚本释放锁。
     */
    @Test
    void paidMessageReleasesLockWithUnlockScript() {
        mockPaidLockAcquired();
        String orderSn = "O-UNLOCK-001";
        Order order = new Order();
        order.setSn(orderSn);
        order.setOrderType(OrderTypeEnum.NORMAL.name());

        OrderItem item = new OrderItem();
        item.setSkuId("sku-1");
        item.setNum(1);

        OrderDetailVO detail = new OrderDetailVO();
        detail.setOrder(order);
        detail.setOrderItems(List.of(item));

        when(orderService.queryDetail(orderSn)).thenReturn(detail);
        when(orderService.getBySn(orderSn)).thenReturn(order);
        when(cache.multiGet(anyList())).thenReturn(List.of(10));
        when(stringRedisTemplate.execute(eq(quantityScript), anyList(), any())).thenReturn(true);

        OrderMessage message = new OrderMessage();
        message.setOrderSn(orderSn);
        message.setNewStatus(OrderStatusEnum.PAID);

        stockUpdateExecute.orderChange(message);

        // 验证不再使用非原子的 delete(lockKey)，保证锁安全
        verify(stringRedisTemplate, never()).delete(anyString());
    }
}
