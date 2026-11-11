package cn.lili.event.impl;

import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.promotion.service.MemberCouponService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 订单取消时优惠券返还单元测试
 *
 * @author Bulbasaur
 * @since 2026-11-11
 */
class MemberCouponExecuteCancelTest {

    private MemberCouponExecute memberCouponExecute;
    private OrderService orderService;
    private MemberCouponService memberCouponService;

    @BeforeEach
    void setUp() {
        memberCouponExecute = new MemberCouponExecute();
        orderService = mock(OrderService.class);
        memberCouponService = mock(MemberCouponService.class);

        ReflectionTestUtils.setField(memberCouponExecute, "orderService", orderService);
        ReflectionTestUtils.setField(memberCouponExecute, "memberCouponService", memberCouponService);
    }

    /**
     * 订单取消时店铺优惠券与平台优惠券均执行退还
     */
    @Test
    @DisplayName("订单取消时店铺优惠券与平台优惠券均执行退还")
    void cancelledOrderRecoversCoupons() {
        Order order = new Order();
        order.setSn("O_COUPON_001");
        order.setOrderStatus(OrderStatusEnum.CANCELLED.name());
        order.setUseStoreMemberCouponIds("store_c1,store_c2");
        order.setUsePlatformMemberCouponId("plat_c1");

        when(orderService.getBySn("O_COUPON_001")).thenReturn(order);

        OrderMessage message = new OrderMessage();
        message.setOrderSn("O_COUPON_001");
        message.setNewStatus(OrderStatusEnum.CANCELLED);

        memberCouponExecute.orderChange(message);

        // 验证店铺优惠券退还
        verify(memberCouponService).recoveryMemberCoupon(eq(List.of("store_c1", "store_c2")));
        // 验证平台优惠券退还
        verify(memberCouponService).recoveryMemberCoupon(eq(List.of("plat_c1")));
    }

    /**
     * 未使用优惠券的订单取消时不调用优惠券退还
     */
    @Test
    @DisplayName("未使用优惠券的订单取消时不调用优惠券退还")
    void orderWithoutCouponsDoesNotRecover() {
        Order order = new Order();
        order.setSn("O_COUPON_002");
        order.setOrderStatus(OrderStatusEnum.CANCELLED.name());
        order.setUseStoreMemberCouponIds(null);
        order.setUsePlatformMemberCouponId(null);

        when(orderService.getBySn("O_COUPON_002")).thenReturn(order);

        OrderMessage message = new OrderMessage();
        message.setOrderSn("O_COUPON_002");
        message.setNewStatus(OrderStatusEnum.CANCELLED);

        memberCouponExecute.orderChange(message);

        verify(memberCouponService, never()).recoveryMemberCoupon(any());
    }
}
