package cn.lili.event.impl;

import cn.lili.cache.Cache;
import cn.lili.modules.member.entity.enums.PointSourceEnum;
import cn.lili.modules.member.entity.enums.PointTypeEnum;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.system.service.SettingService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.concurrent.TimeUnit;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 订单取消时积分返还单元测试
 *
 * @author Bulbasaur
 * @since 2026-11-11
 */
class MemberPointExecuteCancelTest {

    private MemberPointExecute memberPointExecute;
    private MemberService memberService;
    private OrderService orderService;
    private Cache cache;

    @BeforeEach
    void setUp() {
        memberPointExecute = new MemberPointExecute();
        memberService = mock(MemberService.class);
        orderService = mock(OrderService.class);
        SettingService settingService = mock(SettingService.class);
        cache = mock(Cache.class);

        ReflectionTestUtils.setField(memberPointExecute, "memberService", memberService);
        ReflectionTestUtils.setField(memberPointExecute, "orderService", orderService);
        ReflectionTestUtils.setField(memberPointExecute, "settingService", settingService);
        ReflectionTestUtils.setField(memberPointExecute, "cache", cache);

        when(memberService.updateMemberPoint(anyLong(), anyString(), anyString(), anyString(), anyString())).thenReturn(true);
    }

    /**
     * 未付款订单取消时，下单扣减的积分必须退回
     */
    @Test
    @DisplayName("未付款订单取消时下单扣减积分必须退回")
    void unpaidOrderCancelledRefundsDeductedPoints() {
        Order order = new Order();
        order.setSn("O_POINT_001");
        order.setMemberId("M_001");
        order.setOrderStatus(OrderStatusEnum.CANCELLED.name());
        order.setPayStatus(PayStatusEnum.UNPAID.name());

        PriceDetailDTO priceDetailDTO = new PriceDetailDTO();
        priceDetailDTO.setPayPoint(150L);
        order.setPriceDetailDTO(priceDetailDTO);

        when(orderService.getBySn("O_POINT_001")).thenReturn(order);
        when(cache.hasKey("order:point:refund:O_POINT_001")).thenReturn(false);

        OrderMessage message = new OrderMessage();
        message.setOrderSn("O_POINT_001");
        message.setNewStatus(OrderStatusEnum.CANCELLED);

        memberPointExecute.orderChange(message);

        // 验证调用了加积分
        verify(memberService).updateMemberPoint(
                eq(150L),
                eq(PointTypeEnum.INCREASE.name()),
                eq("M_001"),
                eq("订单取消，积分返还：150分"),
                eq(PointSourceEnum.ORDER.name())
        );

        // 验证写入缓存防重
        verify(cache).put(eq("order:point:refund:O_POINT_001"), eq(1), eq(7L), eq(TimeUnit.DAYS));
    }

    /**
     * 重复收到订单取消事件时幂等防重，不重复返还积分
     */
    @Test
    @DisplayName("重复收到订单取消事件时幂等防重不重复返还积分")
    void cancelledOrderPointRefundIsIdempotent() {
        Order order = new Order();
        order.setSn("O_POINT_002");
        order.setMemberId("M_002");
        order.setOrderStatus(OrderStatusEnum.CANCELLED.name());
        order.setPayStatus(PayStatusEnum.UNPAID.name());

        PriceDetailDTO priceDetailDTO = new PriceDetailDTO();
        priceDetailDTO.setPayPoint(200L);
        order.setPriceDetailDTO(priceDetailDTO);

        when(orderService.getBySn("O_POINT_002")).thenReturn(order);
        // 模拟已退还过缓存中存在标记
        when(cache.hasKey("order:point:refund:O_POINT_002")).thenReturn(true);

        OrderMessage message = new OrderMessage();
        message.setOrderSn("O_POINT_002");
        message.setNewStatus(OrderStatusEnum.CANCELLED);

        memberPointExecute.orderChange(message);

        // 验证未再次调用加积分
        verify(memberService, never()).updateMemberPoint(anyLong(), anyString(), anyString(), anyString(), anyString());
    }

    /**
     * 未使用积分的订单取消时不触发积分返还
     */
    @Test
    @DisplayName("未使用积分的订单取消时不触发积分返还")
    void orderWithoutPointsDoesNotTriggerRefund() {
        Order order = new Order();
        order.setSn("O_POINT_003");
        order.setMemberId("M_003");
        order.setOrderStatus(OrderStatusEnum.CANCELLED.name());
        order.setPayStatus(PayStatusEnum.UNPAID.name());

        PriceDetailDTO priceDetailDTO = new PriceDetailDTO();
        priceDetailDTO.setPayPoint(0L);
        order.setPriceDetailDTO(priceDetailDTO);

        when(orderService.getBySn("O_POINT_003")).thenReturn(order);

        OrderMessage message = new OrderMessage();
        message.setOrderSn("O_POINT_003");
        message.setNewStatus(OrderStatusEnum.CANCELLED);

        memberPointExecute.orderChange(message);

        verify(memberService, never()).updateMemberPoint(anyLong(), anyString(), anyString(), anyString(), anyString());
    }
}
