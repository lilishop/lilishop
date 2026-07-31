/**
 * 作者：Mike
 * 日期：2026-07-31
 */
package cn.lili.modules.goods.serviceimpl;

import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.enums.OrderPromotionTypeEnum;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.mapper.OrderMapper;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * SVC-07 fulfillAfterPayment 履约分支单元测试。
 *
 * @author Mike
 * @date 2026-07-31
 */
class CardKeyServiceImplFulfillTest {

    private CardKeyServiceImpl cardKeyService;
    private OrderMapper orderMapper;
    private OrderService orderService;
    private OrderItemService orderItemService;

    @BeforeEach
    void setUp() {
        cardKeyService = spy(new CardKeyServiceImpl());
        orderMapper = mock(OrderMapper.class);
        orderService = mock(OrderService.class);
        orderItemService = mock(OrderItemService.class);
        ReflectionTestUtils.setField(cardKeyService, "orderMapper", orderMapper);
        ReflectionTestUtils.setField(cardKeyService, "orderService", orderService);
        ReflectionTestUtils.setField(cardKeyService, "orderItemService", orderItemService);
    }

    @Test
    void fulfillAfterPaymentSkipsWhenAlreadyCompleted() {
        Order order = purchaseTypeOrder(OrderStatusEnum.COMPLETED.name());
        when(orderMapper.selectOne(any())).thenReturn(order);

        cardKeyService.fulfillAfterPayment("O001");

        verify(orderService, never()).systemComplete(any());
    }

    @Test
    void fulfillAfterPaymentCompletesPurchaseTypeOrder() {
        Order order = purchaseTypeOrder(OrderStatusEnum.PAID.name());
        when(orderMapper.selectOne(any())).thenReturn(order);
        doNothing().when(cardKeyService).allocateByOrder("O001");

        cardKeyService.fulfillAfterPayment("O001");

        verify(cardKeyService).allocateByOrder("O001");
        verify(orderService).systemComplete("O001");
    }

    @Test
    void fulfillAfterPaymentCompletesNonPurchaseTypeWithoutAllocate() {
        Order order = purchaseTypeOrder(OrderStatusEnum.PAID.name());
        order.setOrderPromotionType(OrderPromotionTypeEnum.GIFT.name());
        order.setParentOrderSn("PARENT001");
        when(orderMapper.selectOne(any())).thenReturn(order);

        cardKeyService.fulfillAfterPayment("O001");

        verify(cardKeyService, never()).allocateByOrder(any());
        verify(orderService).systemComplete("O001");
    }

    @Test
    void fulfillAfterPaymentCancelsWhenAllocateFails() {
        Order order = purchaseTypeOrder(OrderStatusEnum.PAID.name());
        when(orderMapper.selectOne(any())).thenReturn(order);
        OrderItem item = new OrderItem();
        item.setSkuId("sku-1");
        when(orderItemService.getByOrderSn("O001")).thenReturn(List.of(item));
        doThrow(new RuntimeException("pool empty")).when(cardKeyService).allocateByOrder("O001");
        doNothing().when(cardKeyService).voidOnSystemFailure("O001");
        doNothing().when(cardKeyService).syncSkuStock("sku-1");

        cardKeyService.fulfillAfterPayment("O001");

        verify(cardKeyService).voidOnSystemFailure("O001");
        verify(cardKeyService).syncSkuStock("sku-1");
        verify(orderService).systemCancel(eq("O001"), eq("库存不足，出库失败"), eq(true));
    }

    @Test
    void fulfillAfterPaymentSkipsNonECouponOrder() {
        Order order = purchaseTypeOrder(OrderStatusEnum.PAID.name());
        order.setOrderType(OrderTypeEnum.VIRTUAL.name());
        when(orderMapper.selectOne(any())).thenReturn(order);

        cardKeyService.fulfillAfterPayment("O001");

        verify(orderService, never()).systemComplete(any());
        verify(cardKeyService, never()).allocateByOrder(any());
    }

    private Order purchaseTypeOrder(String status) {
        Order order = new Order();
        order.setSn("O001");
        order.setOrderType(OrderTypeEnum.E_COUPON.name());
        order.setOrderPromotionType(OrderPromotionTypeEnum.NORMAL.name());
        order.setParentOrderSn("");
        order.setOrderStatus(status);
        return order;
    }
}
