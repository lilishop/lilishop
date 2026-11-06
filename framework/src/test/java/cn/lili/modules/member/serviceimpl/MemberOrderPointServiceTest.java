package cn.lili.modules.member.serviceimpl;

import cn.lili.modules.member.mapper.MemberPointsHistoryMapper;
import cn.lili.modules.member.service.MemberOrderPointService;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.order.aftersale.entity.dos.AfterSale;
import cn.lili.modules.order.aftersale.mapper.AfterSaleMapper;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.member.service.MemberBenefitService;
import cn.lili.modules.system.service.SettingService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.order.order.mapper.OrderItemMapper;
import cn.lili.modules.order.order.mapper.OrderMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;
import java.math.BigDecimal;
import java.time.Instant;
import java.util.Date;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import static org.assertj.core.api.Assertions.*;
import static org.mockito.Mockito.*;

/**
 * 关闭售后后直接赠送积分测试，复用现有积分流水防重，不读取或更新订单积分字段。
 * 模拟流水与事务调用，不代表真实数据库并发及AOP集成验收。
 * @author Bulbasaur
 * @since 2026-11-04
 */
class MemberOrderPointServiceTest {
    /** 模拟普通订单查询。 */
    private final OrderMapper orders=mock(OrderMapper.class);
    /** 模拟商品项售后关闭状态。 */
    private final OrderItemMapper items=mock(OrderItemMapper.class);
    /** 模拟最终售后记录。 */
    private final AfterSaleMapper afterSales=mock(AfterSaleMapper.class);
    /** 模拟已有积分流水。 */
    private final MemberPointsHistoryMapper histories=mock(MemberPointsHistoryMapper.class);
    /** 模拟客户积分变更。 */
    private final MemberService members=mock(MemberService.class);
    /** 模拟发放时的积分设置。 */
    private final SettingService settings=mock(SettingService.class);
    /** 模拟客户当前有效权益最高倍率。 */
    private final MemberBenefitService benefits=mock(MemberBenefitService.class);
    /** 被测试的积分发放服务。 */
    private final MemberOrderPointService service=new MemberOrderPointServiceImpl();
    private final cn.lili.modules.promotion.service.PickupCardOrderService pickupOrders=mock(cn.lili.modules.promotion.service.PickupCardOrderService.class);
    /** 表示积分变动切面已保存现有流水。 */
    private final AtomicBoolean rewarded=new AtomicBoolean();
    /** 持久化订单样本，不包含额外积分状态。 */
    private Order order;
    /** 售后入口状态样本。 */
    private OrderItem item;

    /** 准备已完成且售后入口关闭、原金额十元及1.5倍积分的样本。 */
    @BeforeEach void setup() {
        ReflectionTestUtils.setField(service,"pickupCardOrderService",pickupOrders);
        ReflectionTestUtils.setField(service,"orders",orders); ReflectionTestUtils.setField(service,"items",items);
        ReflectionTestUtils.setField(service,"afterSales",afterSales); ReflectionTestUtils.setField(service,"histories",histories);
        ReflectionTestUtils.setField(service,"memberService",members);
        ReflectionTestUtils.setField(service,"settings",settings); ReflectionTestUtils.setField(service,"benefits",benefits);
        order=new Order(); order.setId("id"); order.setSn("order"); order.setMemberId("member"); order.setFlowPrice(10D);
        order.setOrderStatus("COMPLETED"); order.setCompleteTime(Date.from(Instant.now().minusSeconds(10*86400)));
        item=new OrderItem(); item.setAfterSaleStatus("EXPIRED");
        var setting=new Setting(); setting.setSettingValue("{\"consumer\":1}");
        when(settings.get(SettingEnum.POINT_SETTING.name())).thenReturn(setting);
        when(benefits.getPointMultiplier("member")).thenReturn(new BigDecimal("1.5"));
        when(orders.selectOne(any())).thenReturn(order);
        when(items.selectList(any())).thenReturn(List.of(item));
        when(afterSales.memberPointAfterSales("order")).thenReturn(List.of());
        when(histories.findOrderReward("member","order")).thenAnswer(call -> rewarded.get()?"history":null);
        when(members.updateMemberPoint(anyLong(),anyString(),anyString(),anyString(),anyString()))
                .thenAnswer(call -> { rewarded.set(true); return true; });
    }

    /**
     * 创建最终售后样本。
     * @param status 售后状态
     * @param amount 实际退款金额，单位元
     * @return 售后记录
     */
    private AfterSale refund(String status,double amount) {
        var row=new AfterSale(); row.setServiceStatus(status); row.setActualRefundPrice(amount); return row;
    }

    /** 重复任务查到既有流水后直接跳过，不依赖订单标记。 */
    @Test void existingHistoryPreventsRepeatedAward() {
        service.complete(order.getSn()); service.complete(order.getSn());
        verify(members,times(1)).updateMemberPoint(eq(15L),eq("INCREASE"),eq("member"),anyString(),eq("ORDER"));
        verify(orders,never()).updateById(any(Order.class));
    }

    /** 提货订单即使完成且售后入口关闭，也不发放消费积分；评价奖励不属于此流程。 */
    @Test void pickupOrderDoesNotReceiveShoppingPoints() {
        when(pickupOrders.isPickupOrder(order.getSn())).thenReturn(true);

        assertThat(service.complete(order.getSn())).isTrue();

        verifyNoInteractions(members, histories, items, afterSales, settings, benefits);
    }

    /** 旧发放记录同样阻止重复赠送。 */
    @Test void priorAwardIsNotIssuedAgain() {
        rewarded.set(true); service.complete(order.getSn()); verifyNoInteractions(members,items,afterSales);
    }

    /** 售后是否关闭以商品项状态为准，不再按完成时间和当前设置推算。 */
    @Test void closedAfterSaleStatusIsAuthoritative() {
        order.setCompleteTime(null);
        assertThat(service.complete(order.getSn())).isTrue();
        verify(members).updateMemberPoint(eq(15L), anyString(), anyString(), anyString(), eq("ORDER"));
    }

    /** 售后入口尚未关闭，不发放。 */
    @Test void openAfterSaleEntryBlocksAward() {
        item.setAfterSaleStatus("NOT_APPLIED"); service.complete(order.getSn()); verifyNoInteractions(members);
    }

    /** 未终结或未知的售后状态均阻止发放。 */
    @Test void processingAfterSaleBlocksAward() {
        for(String state:List.of("APPLY","PASS","BUYER_RETURN","SELLER_CONFIRM","WAIT_REFUND","UNKNOWN")) {
            when(afterSales.memberPointAfterSales("order")).thenReturn(List.of(refund(state,0D))); service.complete(order.getSn());
        }
        verifyNoInteractions(members);
    }

    /** 部分退款结束后按最终保留金额计算倍率积分。 */
    @Test void completedRefundsReduceAwardToNetAmount() {
        when(afterSales.memberPointAfterSales("order")).thenReturn(List.of(refund("COMPLETE",4D)));
        service.complete(order.getSn()); verify(members).updateMemberPoint(eq(9L),eq("INCREASE"),anyString(),anyString(),eq("ORDER"));
    }

    /** 拒绝、商家终止及买家取消不计作退款。 */
    @Test void refusedAndCancelledAfterSalesDoNotReduceAward() {
        when(afterSales.memberPointAfterSales("order")).thenReturn(List.of(refund("REFUSE",10D),refund("BUYER_CANCEL",10D),refund("SELLER_TERMINATION",10D)));
        service.complete(order.getSn()); verify(members).updateMemberPoint(eq(15L),anyString(),anyString(),anyString(),anyString());
    }

    /** 全额或超额退款不赠送积分，不写额外订单状态。 */
    @Test void fullRefundDoesNotAwardOrUpdateOrder() {
        when(afterSales.memberPointAfterSales("order")).thenReturn(List.of(refund("COMPLETE",12D)));
        service.complete(order.getSn()); verifyNoInteractions(members); verify(orders,never()).updateById(any(Order.class));
    }

    /** 未完成及积分兑换订单不参与赠送。 */
    @Test void incompleteAndPointsOrdersDoNotAward() {
        order.setOrderStatus("DELIVERED"); service.complete(order.getSn()); order.setOrderStatus("COMPLETED");
        order.setOrderPromotionType("POINTS"); service.complete(order.getSn()); verifyNoInteractions(members);
    }

    /** 非法退款金额不能生成积分。 */
    @Test void invalidRefundAmountIsRejected() {
        when(afterSales.memberPointAfterSales("order")).thenReturn(List.of(refund("COMPLETE",Double.NaN)));
        assertThatThrownBy(()->service.complete(order.getSn())).hasMessage("售后实际退款金额不正确"); verifyNoInteractions(members);
    }

    /** 余额更新失败抛异常，供事务回滚。 */
    @Test void failedBalanceChangeThrows() {
        doReturn(false).when(members).updateMemberPoint(anyLong(),anyString(),anyString(),anyString(),anyString());
        assertThatThrownBy(()->service.complete(order.getSn())).hasMessage("积分更新失败");
    }

    /** 流水切面未落库时必须抛异常回滚余额，避免静默成功导致下次重复赠送。 */
    @Test void missingHistoryRollsBackAward() {
        doReturn(true).when(members).updateMemberPoint(anyLong(),anyString(),anyString(),anyString(),anyString());
        assertThatThrownBy(()->service.complete(order.getSn())).hasMessage("购物积分流水保存失败");
    }

    /** 订单即便有历史快照，也按发放时修改后的比例及当前权益倍率计算。 */
    @Test void usesCurrentSettingsAndMultiplierInsteadOfOrderSnapshot() {
        order.setPriceDetail("{\"memberPointRate\":100,\"memberPointMultiplier\":100}");
        var setting=new Setting(); setting.setSettingValue("{\"consumer\":2}");
        when(settings.get(SettingEnum.POINT_SETTING.name())).thenReturn(setting);
        when(benefits.getPointMultiplier("member")).thenReturn(new BigDecimal("3"));
        assertThat(service.complete(order.getSn())).isTrue();
        verify(members).updateMemberPoint(eq(60L),anyString(),anyString(),anyString(),eq("ORDER"));
    }

    /** 当前积分设置关闭购物赠送时不增加积分，也不查询权益倍率。 */
    @Test void currentZeroRateDoesNotAward() {
        var setting=new Setting(); setting.setSettingValue("{\"consumer\":0}");
        when(settings.get(SettingEnum.POINT_SETTING.name())).thenReturn(setting);
        assertThat(service.complete(order.getSn())).isTrue();
        verifyNoInteractions(members,benefits);
    }

    /** 会员倍率权益失效后按当前一倍计算，不保留下单时的较高倍率。 */
    @Test void expiredMultiplierUsesCurrentOrdinaryRate() {
        when(benefits.getPointMultiplier("member")).thenReturn(BigDecimal.ONE);
        service.complete(order.getSn());
        verify(members).updateMemberPoint(eq(10L),anyString(),anyString(),anyString(),eq("ORDER"));
    }

    /** 积分设置异常时保留失败供重试，不能按零积分静默完成。 */
    @Test void missingCurrentSettingsFailsForRetry() {
        when(settings.get(SettingEnum.POINT_SETTING.name())).thenReturn(null);
        assertThatThrownBy(()->service.complete(order.getSn())).hasMessage("积分设置不存在");
        verifyNoInteractions(members,benefits);
    }
}
