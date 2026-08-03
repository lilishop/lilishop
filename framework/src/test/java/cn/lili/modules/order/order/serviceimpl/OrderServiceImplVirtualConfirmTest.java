package cn.lili.modules.order.order.serviceimpl;

import cn.lili.modules.goods.service.CardKeyService;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

/**
 * virtualOrderConfirm 仅 VIRTUAL+PAID 写 TAKE；E_COUPON 误入时拒绝。
 *
 * @author Mike
 * @date 2026-07-31
 */
class OrderServiceImplVirtualConfirmTest {

    private OrderServiceImpl orderService;
    private CardKeyService cardKeyService;

    @BeforeAll
    static void initOrderEntityCache() {
        MapperBuilderAssistant assistant = new MapperBuilderAssistant(new MybatisConfiguration(), "");
        TableInfoHelper.initTableInfo(assistant, Order.class);
    }

    @BeforeEach
    void setUp() {
        orderService = spy(new OrderServiceImpl());
        cardKeyService = mock(CardKeyService.class);
        ReflectionTestUtils.setField(orderService, "cardKeyService", cardKeyService);
    }

    /** E_COUPON 误入 virtualOrderConfirm 时不转发履约、不写 TAKE。 */
    @Test
    void virtualOrderConfirmRejectsECouponMisroute() {
        Order order = new Order();
        order.setSn("O-ECOUPON");
        order.setOrderType(OrderTypeEnum.E_COUPON.name());
        order.setOrderStatus(OrderStatusEnum.PAID.name());
        doReturn(order).when(orderService).getBySn("O-ECOUPON");

        orderService.virtualOrderConfirm("O-ECOUPON");

        verify(cardKeyService, never()).fulfillAfterPayment(any());
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
        doNothing().when(orderService).sendUpdateStatusMessage(any(OrderMessage.class));

        orderService.virtualOrderConfirm("O-VIRTUAL");

        verify(orderService).update(any(LambdaUpdateWrapper.class));
        verify(orderService).sendUpdateStatusMessage(any(OrderMessage.class));
    }

    /** 未知或空 orderType 不进入任何确认分支。 */
    @Test
    void afterOrderConfirmRejectsUnknownAndNullOrderTypes() {
        Order unknownOrder = new Order();
        unknownOrder.setSn("O-UNKNOWN");
        unknownOrder.setOrderType("UNKNOWN");
        unknownOrder.setOrderStatus(OrderStatusEnum.PAID.name());
        Order nullTypeOrder = new Order();
        nullTypeOrder.setSn("O-NULL-TYPE");
        nullTypeOrder.setOrderStatus(OrderStatusEnum.PAID.name());
        doReturn(unknownOrder).when(orderService).getBySn("O-UNKNOWN");
        doReturn(nullTypeOrder).when(orderService).getBySn("O-NULL-TYPE");

        orderService.afterOrderConfirm("O-UNKNOWN");
        orderService.afterOrderConfirm("O-NULL-TYPE");

        verify(orderService, never()).normalOrderConfirm(any());
        verify(orderService, never()).virtualOrderConfirm(any());
        verify(cardKeyService, never()).fulfillAfterPayment(any());
        verify(cardKeyService, never()).reserveByOrder(any());
    }
}
