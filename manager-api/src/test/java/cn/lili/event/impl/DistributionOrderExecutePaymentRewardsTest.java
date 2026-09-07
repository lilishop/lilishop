package cn.lili.event.impl;

import cn.lili.modules.distribution.service.DistributionOrderService;
import cn.lili.modules.distribution.service.DistributionRewardService;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 订单状态 MQ 补偿入账：已有奖励则跳过，失败后可补。
 *
 * @author Mike
 * @date 2026-09-07
 */
class DistributionOrderExecutePaymentRewardsTest {

    private DistributionOrderExecute execute;
    private DistributionOrderService distributionOrderService;
    private DistributionRewardService distributionRewardService;

    @BeforeEach
    void setUp() {
        execute = new DistributionOrderExecute();
        distributionOrderService = mock(DistributionOrderService.class);
        distributionRewardService = mock(DistributionRewardService.class);
        ReflectionTestUtils.setField(execute, "distributionOrderService", distributionOrderService);
        ReflectionTestUtils.setField(execute, "distributionRewardService", distributionRewardService);
    }

    /**
     * afterCommit 已入账时，MQ 补偿跳过。
     */
    @Test
    void paidSkipsWhenAfterCommitAlreadyWroteRewards() {
        when(distributionRewardService.hasRewardsByOrderSn("O001")).thenReturn(true);

        // 已有奖励，补偿路径不应再入账
        execute.orderChange(paidMessage());

        verify(distributionOrderService, never()).handlePaymentRewards("O001");
    }

    /**
     * afterCommit 未写入时，PAID 消息补偿入账。
     */
    @Test
    void paidCompensatesWhenAfterCommitDidNotWrite() {
        when(distributionRewardService.hasRewardsByOrderSn("O001")).thenReturn(false);

        // afterCommit 未写入，PAID 消息补偿入账
        execute.orderChange(paidMessage());

        verify(distributionOrderService).handlePaymentRewards("O001");
    }

    /**
     * 待发货状态缺失奖励时同样补偿入账。
     */
    @Test
    void undeliveredAlsoCompensatesWhenMissingRewards() {
        when(distributionRewardService.hasRewardsByOrderSn("O001")).thenReturn(false);
        OrderMessage message = paidMessage();
        message.setNewStatus(OrderStatusEnum.UNDELIVERED);

        // 待发货同样走补偿
        execute.orderChange(message);

        verify(distributionOrderService).handlePaymentRewards("O001");
    }

    /**
     * 构造已支付订单消息。
     */
    private static OrderMessage paidMessage() {
        OrderMessage message = new OrderMessage();
        message.setOrderSn("O001");
        message.setNewStatus(OrderStatusEnum.PAID);
        message.setPaymentMethod("WECHAT");
        return message;
    }
}
