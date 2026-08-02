package cn.lili.modules.order.order.serviceimpl;

import cn.lili.modules.goods.service.CardKeyService;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

/**
 * virtualOrderConfirm 对 E_COUPON 转发 fulfillAfterPayment，VIRTUAL+PAID 才写 TAKE。
 *
 * @author Mike
 * @date 2026-07-31
 */
class OrderServiceImplVirtualConfirmTest {

    private OrderServiceImpl orderService;
    private CardKeyService cardKeyService;

    @BeforeEach
    void setUp() {
        orderService = spy(new OrderServiceImpl());
        cardKeyService = mock(CardKeyService.class);
        ReflectionTestUtils.setField(orderService, "cardKeyService", cardKeyService);
    }

    @Test
    void virtualOrderConfirmForwardsECouponToFulfill() {
        Order order = new Order();
        order.setSn("O-ECOUPON");
        order.setOrderType(OrderTypeEnum.E_COUPON.name());
        order.setOrderStatus(OrderStatusEnum.PAID.name());
        doReturn(order).when(orderService).getBySn("O-ECOUPON");

        orderService.virtualOrderConfirm("O-ECOUPON");

        verify(cardKeyService).fulfillAfterPayment("O-ECOUPON");
        verify(orderService, never()).update(any(LambdaUpdateWrapper.class));
        verify(orderService, never()).sendUpdateStatusMessage(any(OrderMessage.class));
    }

    @Test
    void virtualOrderConfirmSkipsTakeWhenCasUpdateFails() {
        Order order = new Order();
        order.setSn("O-VIRTUAL");
        order.setOrderType(OrderTypeEnum.VIRTUAL.name());
        order.setOrderStatus(OrderStatusEnum.COMPLETED.name());
        doReturn(order).when(orderService).getBySn("O-VIRTUAL");
        doReturn(false).when(orderService).update(any(LambdaUpdateWrapper.class));

        orderService.virtualOrderConfirm("O-VIRTUAL");

        verify(cardKeyService, never()).fulfillAfterPayment(any());
        verify(orderService, never()).sendUpdateStatusMessage(any(OrderMessage.class));
    }

    @Test
    void virtualOrderConfirmSetsTakeOnlyForVirtualPaid() {
        Order order = new Order();
        order.setSn("O-VIRTUAL");
        order.setOrderType(OrderTypeEnum.VIRTUAL.name());
        order.setOrderStatus(OrderStatusEnum.PAID.name());
        doReturn(order).when(orderService).getBySn("O-VIRTUAL");
        doReturn(true).when(orderService).update(any(LambdaUpdateWrapper.class));

        orderService.virtualOrderConfirm("O-VIRTUAL");

        verify(orderService).update(any(LambdaUpdateWrapper.class));
        verify(orderService).sendUpdateStatusMessage(any(OrderMessage.class));
    }
}
