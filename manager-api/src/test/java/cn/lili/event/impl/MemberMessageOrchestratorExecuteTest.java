package cn.lili.event.impl;

import cn.lili.message.MemberMessageOrchestrator;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dto.MemberPointMessage;
import cn.lili.modules.member.entity.enums.PointTypeEnum;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.message.entity.dto.NoticeMessageDTO;
import cn.lili.modules.message.entity.enums.NoticeMessageNodeEnum;
import cn.lili.modules.order.aftersale.entity.dos.AfterSale;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.vo.OrderDetailVO;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.wallet.entity.dto.MemberWithdrawalMessage;
import cn.lili.modules.wallet.entity.enums.WithdrawStatusEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Date;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatCode;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * 事件到消息编排器的映射执行器
 */
class MemberMessageOrchestratorExecuteTest {

    private MemberMessageOrchestratorExecute execute;
    private MemberMessageOrchestrator orchestrator;
    private OrderService orderService;
    private MemberService memberService;

    /**
     * 初始化当前用例的模拟依赖及测试数据。
     */
    @BeforeEach
    void setUp() {
        execute = new MemberMessageOrchestratorExecute();
        orchestrator = mock(MemberMessageOrchestrator.class);
        orderService = mock(OrderService.class);
        memberService = mock(MemberService.class);
        ReflectionTestUtils.setField(execute, "memberMessageOrchestrator", orchestrator);
        ReflectionTestUtils.setField(execute, "orderService", orderService);
        ReflectionTestUtils.setField(execute, "memberService", memberService);
    }

    /**
     * 构造通知渲染所需的订单详情。
     *
     * @return 通知渲染所需的订单详情
     */
    private OrderDetailVO orderDetail() {
        Order order = new Order();
        order.setSn("SN123");
        order.setMemberId("m1");
        order.setMemberName("张三");
        order.setStoreName("旗舰店");
        order.setFlowPrice(99.5);
        order.setLogisticsName("顺丰");
        order.setLogisticsNo("SF001");
        order.setLogisticsTime(new Date());
        order.setCompleteTime(new Date());
        OrderItem item = new OrderItem();
        item.setGoodsName("手机");
        item.setNum(2);
        OrderDetailVO vo = new OrderDetailVO();
        vo.setOrder(order);
        vo.setOrderItems(List.of(item));
        return vo;
    }

    /**
     * 构造指定订单状态的变更消息。
     *
     * @param status 测试状态
     * @return 指定订单状态的变更消息
     */
    private OrderMessage orderMessage(OrderStatusEnum status) {
        OrderMessage message = new OrderMessage();
        message.setOrderSn("SN123");
        message.setNewStatus(status);
        return message;
    }

    /**
     * 验证订单发货事件映射为发货通知场景。
     */
    @Test
    void deliveredMapsToOrderDeliver() {
        when(orderService.queryDetail("SN123")).thenReturn(orderDetail());

        execute.orderChange(orderMessage(OrderStatusEnum.DELIVERED));

        ArgumentCaptor<NoticeMessageDTO> captor = ArgumentCaptor.forClass(NoticeMessageDTO.class);
        verify(orchestrator).orchestrate(captor.capture());
        NoticeMessageDTO dto = captor.getValue();
        assertThat(dto.getNoticeMessageNodeEnum()).isEqualTo(NoticeMessageNodeEnum.ORDER_DELIVER);
        assertThat(dto.getMemberId()).isEqualTo("m1");
        assertThat(dto.getParameter()).containsEntry("order_sn", "SN123")
                .containsEntry("logistics_no", "SF001")
                .containsEntry("goods", "手机")
                .containsEntry("store_name", "旗舰店");
        assertThat(dto.getPage()).isEqualTo("pages/order/orderDetail?sn=SN123");
    }

    /**
     * 验证付款及取消事件不在该通知执行器中重复发送。
     */
    @Test
    void paidAndCancelledSendNothing() {
        when(orderService.queryDetail("SN123")).thenReturn(orderDetail());

        execute.orderChange(orderMessage(OrderStatusEnum.PAID));
        execute.orderChange(orderMessage(OrderStatusEnum.CANCELLED));

        verifyNoInteractions(orchestrator);
    }

    /**
     * 验证售后拒绝事件映射为审核结果通知。
     */
    @Test
    void afterSaleRefuseMapsAuditResult() {
        AfterSale afterSale = new AfterSale();
        afterSale.setMemberId("m1");
        afterSale.setSn("AS001");
        afterSale.setOrderSn("SN123");
        afterSale.setGoodsName("手机");
        afterSale.setServiceStatus("REFUSE");
        afterSale.setAuditRemark("不符合退款条件");

        execute.afterSaleStatusChange(afterSale);

        ArgumentCaptor<NoticeMessageDTO> captor = ArgumentCaptor.forClass(NoticeMessageDTO.class);
        verify(orchestrator).orchestrate(captor.capture());
        NoticeMessageDTO dto = captor.getValue();
        assertThat(dto.getNoticeMessageNodeEnum()).isEqualTo(NoticeMessageNodeEnum.AFTER_SALE_AUDIT);
        assertThat(dto.getParameter()).containsEntry("audit_result", "审核未通过")
                .containsEntry("after_sale_sn", "AS001");
        assertThat(dto.getPage()).isEqualTo("pages/order/afterSales/applyDetail?sn=AS001");
    }

    /**
     * 验证积分变动通知填入会员当前积分。
     */
    @Test
    void pointChangeFillsCurrentPoint() {
        Member member = new Member();
        member.setPoint(100L);
        when(memberService.getById("m1")).thenReturn(member);

        MemberPointMessage message = new MemberPointMessage();
        message.setMemberId("m1");
        message.setPoint(5L);
        message.setType(PointTypeEnum.INCREASE.name());

        execute.memberPointChange(message);

        ArgumentCaptor<NoticeMessageDTO> captor = ArgumentCaptor.forClass(NoticeMessageDTO.class);
        verify(orchestrator).orchestrate(captor.capture());
        NoticeMessageDTO dto = captor.getValue();
        assertThat(dto.getNoticeMessageNodeEnum()).isEqualTo(NoticeMessageNodeEnum.POINT_CHANGE);
        assertThat(dto.getParameter()).containsEntry("current_point", "100")
                .containsEntry("income_points", "5")
                .containsEntry("expenditure_points", "0");
        assertThat(dto.getPage()).isEqualTo("pages/mine/point/myPoint");
    }

    /**
     * 验证提现申请及成功事件不在该执行器中重复发送。
     */
    @Test
    void withdrawalApplyAndSuccessSendNothing() {
        MemberWithdrawalMessage apply = new MemberWithdrawalMessage();
        apply.setMemberId("m1");
        apply.setPrice(10.0);
        apply.setStatus(WithdrawStatusEnum.APPLY.name());
        execute.memberWithdrawal(apply);

        MemberWithdrawalMessage success = new MemberWithdrawalMessage();
        success.setMemberId("m1");
        success.setPrice(10.0);
        success.setStatus(WithdrawStatusEnum.SUCCESS.name());
        execute.memberWithdrawal(success);

        verifyNoInteractions(orchestrator);
    }

    /**
     * 验证提现审核失败映射为审核错误通知。
     */
    @Test
    void withdrawalFailAuditMapsAuditError() {
        MemberWithdrawalMessage message = new MemberWithdrawalMessage();
        message.setMemberId("m1");
        message.setPrice(10.0);
        message.setStatus(WithdrawStatusEnum.FAIL_AUDITING.name());

        execute.memberWithdrawal(message);

        ArgumentCaptor<NoticeMessageDTO> captor = ArgumentCaptor.forClass(NoticeMessageDTO.class);
        verify(orchestrator).orchestrate(captor.capture());
        NoticeMessageDTO dto = captor.getValue();
        assertThat(dto.getNoticeMessageNodeEnum()).isEqualTo(NoticeMessageNodeEnum.WALLET_WITHDRAWAL_AUDIT_ERROR);
        assertThat(dto.getParameter()).containsEntry("audit_result", "审核未通过")
                .containsEntry("price", "10.0");
        assertThat(dto.getPage()).isEqualTo("pages/mine/deposit/withdrawApply");
    }

    /**
     * 验证通知所需订单详情缺失时不抛异常。
     */
    @Test
    void orderDetailMissingDoesNotThrow() {
        when(orderService.queryDetail("SN123")).thenReturn(null);

        assertThatCode(() -> execute.orderChange(orderMessage(OrderStatusEnum.DELIVERED)))
                .doesNotThrowAnyException();
        verifyNoInteractions(orchestrator);
    }
}
