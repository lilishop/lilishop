package cn.lili.event.impl;

import cn.lili.modules.member.entity.dos.MemberEvaluation;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.member.entity.enums.PointSourceEnum;
import cn.lili.modules.member.entity.enums.PointTypeEnum;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.order.service.TradeService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.test.util.ReflectionTestUtils;

import static org.mockito.Mockito.*;

/**
 * 提货卡异步事件边界测试，防止重复自动支付，确保评价积分沿用统一配置。
 *
 * @author Bulbasaur
 * @since 2026-11-05
 */
class PickupCardEventsTest {
    /**
     * 兑换事务内已支付，订单创建消息不得再触发一次支付。
     */
    @Test
    void creationEventDoesNotPayPickupAgain() {
        OrderStatusHandlerExecute handler = new OrderStatusHandlerExecute();
        TradeService trades = mock(TradeService.class);
        ReflectionTestUtils.setField(handler, "tradeService", trades);
        TradeDTO trade = new TradeDTO();
        trade.setPickupCardId("card");
        handler.orderCreate(trade);
        verifyNoInteractions(trades);
    }

    /**
     * 普通订单与提货卡订单均按评价积分配置奖励，不再查询订单类型来排除评价奖励。
     */
    @ParameterizedTest
    @NullSource
    @ValueSource(strings = {"card"})
    void reviewGrantsConfiguredPointsRegardlessOfPickupCard(String pickupCardId) {
        MemberPointExecute handler = new MemberPointExecute();
        OrderService orders = mock(OrderService.class);
        MemberService members = mock(MemberService.class);
        SettingService settings = mock(SettingService.class);
        ReflectionTestUtils.setField(handler, "orderService", orders);
        ReflectionTestUtils.setField(handler, "memberService", members);
        ReflectionTestUtils.setField(handler, "settingService", settings);
        Order order = new cn.lili.modules.order.order.entity.vo.OrderInfoVO(new Order(), pickupCardId);
        when(orders.getBySn("order")).thenReturn(order);
        Setting pointSetting = new Setting();
        pointSetting.setSettingValue("{\"comment\":17}");
        when(settings.get(SettingEnum.POINT_SETTING.name())).thenReturn(pointSetting);
        MemberEvaluation review = new MemberEvaluation();
        review.setOrderNo("order");
        review.setMemberId("member");
        handler.goodsComment(review);
        verify(members).updateMemberPoint(17L, PointTypeEnum.INCREASE.name(), "member",
                "客户评价，赠送积分17分", PointSourceEnum.COMMENT.name());
        verifyNoMoreInteractions(members);
        verifyNoInteractions(orders);
    }
}
