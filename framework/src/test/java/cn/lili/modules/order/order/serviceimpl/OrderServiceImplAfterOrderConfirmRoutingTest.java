package cn.lili.modules.order.order.serviceimpl;

import cn.lili.modules.goods.service.CardKeyService;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.enums.OrderPromotionTypeEnum;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

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
 * afterOrderConfirm 路由：E_COUPON 仅进入专用入口，NORMAL/VIRTUAL 不调用卡密服务。
 *
 * @author Mike
 * @date 2026-08-03
 */
class OrderServiceImplAfterOrderConfirmRoutingTest {

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

    /** 购买型 E_COUPON 进入 fulfillAfterPayment。 */
    @Test
    void afterOrderConfirmRoutesPurchaseECouponToFulfillAfterPayment() {
        Order order = eCouponOrder("O-ECOUPON", OrderPromotionTypeEnum.NORMAL);
        doReturn(order).when(orderService).getBySn("O-ECOUPON");
        when(cardKeyService.isECouponFulfillmentEligible(order)).thenReturn(true);

        orderService.afterOrderConfirm("O-ECOUPON");

        verify(cardKeyService).fulfillAfterPayment("O-ECOUPON");
        verify(cardKeyService, never()).reserveByOrder(any());
        verify(orderService, never()).normalOrderConfirm(any());
        verify(orderService, never()).virtualOrderConfirm(any());
    }

    /** 拼团 E_COUPON 进入 reserveByOrder + checkPintuanOrder。 */
    @Test
    void afterOrderConfirmRoutesPintuanECouponToReserveAndCheckPintuan() {
        Order order = eCouponOrder("PT-001", OrderPromotionTypeEnum.PINTUAN);
        order.setPromotionId("act-1");
        doReturn(order).when(orderService).getBySn("PT-001");
        when(cardKeyService.isECouponFulfillmentEligible(order)).thenReturn(true);
        doNothing().when(orderService).checkPintuanOrder(eq("act-1"), eq("PT-001"));

        orderService.afterOrderConfirm("PT-001");

        verify(cardKeyService).reserveByOrder("PT-001");
        verify(cardKeyService, never()).fulfillAfterPayment(any());
        verify(orderService).checkPintuanOrder("act-1", "PT-001");
    }

    /** 履约门禁不满足时跳过卡密服务。 */
    @Test
    void afterOrderConfirmSkipsECouponWhenStateGuardFails() {
        Order order = eCouponOrder("O-BAD", OrderPromotionTypeEnum.NORMAL);
        order.setOrderStatus(OrderStatusEnum.CANCELLED.name());
        doReturn(order).when(orderService).getBySn("O-BAD");
        when(cardKeyService.isECouponFulfillmentEligible(order)).thenReturn(false);

        orderService.afterOrderConfirm("O-BAD");

        verify(cardKeyService, never()).fulfillAfterPayment(any());
        verify(cardKeyService, never()).reserveByOrder(any());
    }

    /** NORMAL 订单走 normalOrderConfirm，不调用卡密服务。 */
    @Test
    void afterOrderConfirmRoutesNormalWithoutCardKeyService() {
        Order order = new Order();
        order.setSn("O-NORMAL");
        order.setOrderType(OrderTypeEnum.NORMAL.name());
        order.setOrderPromotionType(OrderPromotionTypeEnum.NORMAL.name());
        order.setOrderStatus(OrderStatusEnum.PAID.name());
        doReturn(order).when(orderService).getBySn("O-NORMAL");
        doNothing().when(orderService).normalOrderConfirm("O-NORMAL");

        orderService.afterOrderConfirm("O-NORMAL");

        verify(orderService).normalOrderConfirm("O-NORMAL");
        verify(cardKeyService, never()).fulfillAfterPayment(any());
    }

    private Order eCouponOrder(String sn, OrderPromotionTypeEnum promotionType) {
        Order order = new Order();
        order.setSn(sn);
        order.setOrderType(OrderTypeEnum.E_COUPON.name());
        order.setOrderPromotionType(promotionType.name());
        order.setOrderStatus(OrderStatusEnum.PAID.name());
        order.setPayStatus(PayStatusEnum.PAID.name());
        return order;
    }
}
