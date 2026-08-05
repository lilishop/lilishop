package cn.lili.modules.goods.serviceimpl;

import cn.lili.modules.goods.entity.dos.CardKey;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.mapper.CardKeyMapper;
import cn.lili.modules.goods.service.GiftFulfillmentAlertPublisher;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.enums.OrderPromotionTypeEnum;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.mapper.OrderMapper;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.trade.entity.dos.OrderLog;
import cn.lili.modules.order.trade.service.OrderLogService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Collections;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * fulfillAfterPayment 履约分支单元测试。
 */
class CardKeyServiceImplFulfillTest {

    private CardKeyServiceImpl cardKeyService;
    private OrderMapper orderMapper;
    private OrderService orderService;
    private OrderItemService orderItemService;
    private OrderLogService orderLogService;
    private GiftFulfillmentAlertPublisher giftFulfillmentAlertPublisher;
    private CardKeyMapper cardKeyMapper;
    private GoodsSkuService goodsSkuService;

    @BeforeEach
    void setUp() {
        cardKeyService = spy(new CardKeyServiceImpl());
        orderMapper = mock(OrderMapper.class);
        orderService = mock(OrderService.class);
        orderItemService = mock(OrderItemService.class);
        orderLogService = mock(OrderLogService.class);
        giftFulfillmentAlertPublisher = mock(GiftFulfillmentAlertPublisher.class);
        cardKeyMapper = mock(CardKeyMapper.class);
        goodsSkuService = mock(GoodsSkuService.class);
        ReflectionTestUtils.setField(cardKeyService, "orderMapper", orderMapper);
        ReflectionTestUtils.setField(cardKeyService, "orderService", orderService);
        ReflectionTestUtils.setField(cardKeyService, "orderItemService", orderItemService);
        ReflectionTestUtils.setField(cardKeyService, "orderLogService", orderLogService);
        ReflectionTestUtils.setField(cardKeyService, "giftFulfillmentAlertPublisher", giftFulfillmentAlertPublisher);
        ReflectionTestUtils.setField(cardKeyService, "baseMapper", cardKeyMapper);
        ReflectionTestUtils.setField(cardKeyService, "goodsSkuService", goodsSkuService);
        doReturn(0L).when(cardKeyService).count(any());
        doReturn(true).when(cardKeyService).updateById(any());
        doNothing().when(cardKeyService).syncSkuStock(any());
        when(orderService.systemCompleteECoupon(any())).thenReturn(true);
    }

    @Test
    void fulfillAfterPaymentWaitsForPintuanGroup() {
        Order order = pintuanOrder(OrderStatusEnum.PAID.name());
        when(orderMapper.selectForUpdateBySn("PT-001")).thenReturn(order);

        cardKeyService.fulfillAfterPayment("PT-001");

        verify(orderService, never()).systemCompleteECoupon(any());
        verify(orderService, never()).systemCancelECoupon(any(), any(), any(Boolean.class));
    }

    @Test
    void fulfillAfterPaymentSkipsWhenAlreadyCompleted() {
        Order order = purchaseTypeOrder(OrderStatusEnum.COMPLETED.name());
        when(orderMapper.selectForUpdateBySn("O001")).thenReturn(order);

        cardKeyService.fulfillAfterPayment("O001");

        verify(orderService, never()).systemCompleteECoupon(any());
    }

    @Test
    void fulfillAfterPaymentRejectsWhenPayStatusNotPaid() {
        Order order = purchaseTypeOrder(OrderStatusEnum.PAID.name());
        order.setPayStatus(PayStatusEnum.UNPAID.name());
        when(orderMapper.selectForUpdateBySn("O001")).thenReturn(order);

        cardKeyService.fulfillAfterPayment("O001");

        verify(orderService, never()).systemCompleteECoupon(any());
    }

    @Test
    void fulfillAfterPaymentCompletesPurchaseTypeOrder() {
        Order order = purchaseTypeOrder(OrderStatusEnum.PAID.name());
        when(orderMapper.selectForUpdateBySn("O001")).thenReturn(order);
        stubSuccessfulAllocate("O001", "sku-1");

        cardKeyService.fulfillAfterPayment("O001");

        verify(orderService).systemCompleteECoupon("O001");
    }

    @Test
    void fulfillAfterPaymentCompletesPointsOrder() {
        Order order = purchaseTypeOrder(OrderStatusEnum.PAID.name());
        order.setOrderPromotionType(OrderPromotionTypeEnum.POINTS.name());
        when(orderMapper.selectForUpdateBySn("O001")).thenReturn(order);
        stubSuccessfulAllocate("O001", "sku-1");

        cardKeyService.fulfillAfterPayment("O001");

        verify(orderService).systemCompleteECoupon("O001");
    }

    @Test
    void fulfillAfterPaymentCompletesKanjiaOrder() {
        Order order = purchaseTypeOrder(OrderStatusEnum.PAID.name());
        order.setOrderPromotionType(OrderPromotionTypeEnum.KANJIA.name());
        when(orderMapper.selectForUpdateBySn("O001")).thenReturn(order);
        stubSuccessfulAllocate("O001", "sku-1");

        cardKeyService.fulfillAfterPayment("O001");

        verify(orderService).systemCompleteECoupon("O001");
    }

    @Test
    void fulfillAfterPaymentAllocatesAndCompletesGiftOrder() {
        Order gift = giftOrder(OrderStatusEnum.PAID.name());
        when(orderMapper.selectForUpdateBySn("G001")).thenReturn(gift);
        stubSuccessfulAllocate("G001", "sku-gift");

        cardKeyService.fulfillAfterPayment("G001");

        verify(orderService).systemCompleteECoupon("G001");
        verify(orderService, never()).systemCancelECoupon(any(), any(), any());
    }

    @Test
    void fulfillAfterPaymentCancelsGiftWithoutRefundAndWritesLogs() {
        Order gift = giftOrder(OrderStatusEnum.PAID.name());
        Order parent = purchaseTypeOrder(OrderStatusEnum.COMPLETED.name());
        parent.setSn("P001");
        when(orderMapper.selectForUpdateBySn("G001")).thenReturn(gift);
        when(orderMapper.selectOne(any())).thenReturn(parent);
        OrderItem item = new OrderItem();
        item.setSkuId("sku-gift");
        item.setNum(1);
        item.setSn("gi-1");
        when(orderItemService.getByOrderSn("G001")).thenReturn(List.of(item));
        GoodsSku sku = eCouponSku("sku-gift");
        when(goodsSkuService.getById("sku-gift")).thenReturn(sku);
        when(cardKeyMapper.selectForAllocate("sku-gift", 1)).thenReturn(Collections.emptyList());
        doNothing().when(cardKeyService).voidOnSystemFailure("G001");

        cardKeyService.fulfillAfterPayment("G001");

        verify(orderService).systemCancelECoupon(eq("G001"), eq("满赠电子卡券发放失败"), eq(false));
        verify(giftFulfillmentAlertPublisher).onGiftFulfillmentFailure(eq(gift), eq(parent), any());
        ArgumentCaptor<OrderLog> logCaptor = ArgumentCaptor.forClass(OrderLog.class);
        verify(orderLogService, org.mockito.Mockito.atLeastOnce()).save(logCaptor.capture());
        assertThat(logCaptor.getAllValues()).anyMatch(log -> log.getMessage().contains("赠品子单已取消"));
        assertThat(logCaptor.getAllValues()).anyMatch(log -> "P001".equals(log.getOrderSn()));
    }

    @Test
    void fulfillAfterPaymentCancelsWhenPurchaseAllocateFails() {
        Order order = purchaseTypeOrder(OrderStatusEnum.PAID.name());
        when(orderMapper.selectForUpdateBySn("O001")).thenReturn(order);
        OrderItem item = new OrderItem();
        item.setSkuId("sku-1");
        item.setNum(1);
        item.setSn("i-1");
        when(orderItemService.getByOrderSn("O001")).thenReturn(List.of(item));
        when(goodsSkuService.getById("sku-1")).thenReturn(eCouponSku("sku-1"));
        when(cardKeyMapper.selectForAllocate("sku-1", 1)).thenReturn(Collections.emptyList());
        doNothing().when(cardKeyService).voidOnSystemFailure("O001");

        cardKeyService.fulfillAfterPayment("O001");

        verify(cardKeyService, org.mockito.Mockito.atLeastOnce()).voidOnSystemFailure("O001");
        verify(orderService).systemCancelECoupon(eq("O001"), eq("库存不足，出库失败"), eq(true));
    }

    @Test
    void fulfillAfterPaymentSkipsNonECouponOrder() {
        Order order = purchaseTypeOrder(OrderStatusEnum.PAID.name());
        order.setOrderType(OrderTypeEnum.VIRTUAL.name());
        when(orderMapper.selectForUpdateBySn("O001")).thenReturn(order);

        cardKeyService.fulfillAfterPayment("O001");

        verify(orderService, never()).systemCompleteECoupon(any());
    }

    private void stubSuccessfulAllocate(String orderSn, String skuId) {
        OrderItem item = new OrderItem();
        item.setSkuId(skuId);
        item.setNum(1);
        item.setSn("item-1");
        when(orderItemService.getByOrderSn(orderSn)).thenReturn(List.of(item));
        when(goodsSkuService.getById(skuId)).thenReturn(eCouponSku(skuId));
        CardKey cardKey = new CardKey();
        cardKey.setId("card-1");
        cardKey.setSkuId(skuId);
        when(cardKeyMapper.selectForAllocate(skuId, 1)).thenReturn(List.of(cardKey));
    }

    private GoodsSku eCouponSku(String skuId) {
        GoodsSku sku = new GoodsSku();
        sku.setId(skuId);
        sku.setGoodsType(GoodsTypeEnum.E_COUPON.name());
        return sku;
    }

    private Order purchaseTypeOrder(String status) {
        Order order = new Order();
        order.setSn("O001");
        order.setOrderType(OrderTypeEnum.E_COUPON.name());
        order.setOrderPromotionType(OrderPromotionTypeEnum.NORMAL.name());
        order.setParentOrderSn("");
        order.setOrderStatus(status);
        order.setPayStatus(PayStatusEnum.PAID.name());
        order.setMemberId("m1");
        return order;
    }

    private Order giftOrder(String status) {
        Order order = new Order();
        order.setSn("G001");
        order.setOrderType(OrderTypeEnum.E_COUPON.name());
        order.setOrderPromotionType(OrderPromotionTypeEnum.GIFT.name());
        order.setParentOrderSn("P001");
        order.setOrderStatus(status);
        order.setPayStatus(PayStatusEnum.PAID.name());
        order.setMemberId("m1");
        return order;
    }

    private Order pintuanOrder(String status) {
        Order order = new Order();
        order.setSn("PT-001");
        order.setOrderType(OrderTypeEnum.E_COUPON.name());
        order.setOrderPromotionType(OrderPromotionTypeEnum.PINTUAN.name());
        order.setOrderStatus(status);
        order.setPayStatus(PayStatusEnum.PAID.name());
        order.setMemberId("m1");
        return order;
    }
}
