package cn.lili.modules.distribution.listener;

import cn.lili.common.event.TransactionCommitSendMQEvent;
import cn.lili.common.listener.TransactionCommitSendMQListener;
import cn.lili.message.LiliMessageTemplate;
import cn.lili.message.MessageSendCallback;
import cn.lili.modules.distribution.event.DistributionPaymentRewardsEvent;
import cn.lili.modules.distribution.service.DistributionPaymentRewardsFacade;
import cn.lili.modules.distribution.service.DistributionRewardService;
import cn.lili.rocketmq.tags.OrderTagsEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.event.TransactionPhase;
import org.springframework.transaction.event.TransactionalEventListener;

import java.lang.reflect.Method;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

/**
 * 分销入账提交后事件：监听器吞异常、门面单独开 REQUIRES_NEW。
 *
 * @author Mike
 * @date 2026-09-07
 */
class DistributionPaymentRewardsListenerTest {

    private DistributionPaymentRewardsListener listener;
    private DistributionPaymentRewardsFacade facade;

    @BeforeEach
    void setUp() {
        listener = new DistributionPaymentRewardsListener();
        facade = mock(DistributionPaymentRewardsFacade.class);
        ReflectionTestUtils.setField(listener, "distributionPaymentRewardsFacade", facade);
    }

    /**
     * 事件只携带订单号。
     */
    @Test
    void eventCarriesOnlyOrderSn() {
        DistributionPaymentRewardsEvent event = new DistributionPaymentRewardsEvent(this, "O001");
        assertEquals("O001", event.getOrderSn());
    }

    /**
     * 监听器本身无事务，且在支付事务 afterCommit 执行。
     */
    @Test
    void listenerHasNoTransactionalAndRunsAfterCommit() throws NoSuchMethodException {
        Method method = DistributionPaymentRewardsListener.class.getMethod(
                "onPaymentCommitted", DistributionPaymentRewardsEvent.class);
        assertFalse(method.isAnnotationPresent(Transactional.class));
        assertFalse(DistributionPaymentRewardsListener.class.isAnnotationPresent(Transactional.class));
        TransactionalEventListener listenerAnnotation = method.getAnnotation(TransactionalEventListener.class);
        assertNotNull(listenerAnnotation);
        assertEquals(TransactionPhase.AFTER_COMMIT, listenerAnnotation.phase());
    }

    /**
     * 门面以 REQUIRES_NEW 独立开事务。
     */
    @Test
    void facadeOpensRequiresNew() throws NoSuchMethodException {
        Method method = DistributionPaymentRewardsFacade.class.getMethod("handlePaymentRewards", String.class);
        Transactional transactional = method.getAnnotation(Transactional.class);
        assertNotNull(transactional);
        assertEquals(Propagation.REQUIRES_NEW, transactional.propagation());
    }

    /**
     * 门面委托奖励服务入账。
     */
    @Test
    void facadeDelegatesToRewardService() {
        DistributionRewardService rewardService = mock(DistributionRewardService.class);
        DistributionPaymentRewardsFacade realFacade = new DistributionPaymentRewardsFacade();
        ReflectionTestUtils.setField(realFacade, "distributionRewardService", rewardService);

        // 门面转调奖励服务
        realFacade.handlePaymentRewards("O001");

        verify(rewardService).handlePaymentRewards("O001");
    }

    /**
     * 门面失败时监听器吞异常、不外抛。
     */
    @Test
    void listenerSwallowsFacadeFailure() {
        doThrow(new RuntimeException("wallet log column mismatch"))
                .when(facade).handlePaymentRewards("O001");

        // 门面抛错时监听器不得外抛
        assertDoesNotThrow(() -> listener.onPaymentCommitted(
                new DistributionPaymentRewardsEvent(this, "O001")));
        verify(facade).handlePaymentRewards("O001");
    }

    /**
     * 入账失败不打断后续 afterCommit 的订单状态 MQ。
     */
    @Test
    void facadeFailureDoesNotInterruptLaterAfterCommitMq() {
        doThrow(new RuntimeException("wallet log column mismatch"))
                .when(facade).handlePaymentRewards("O001");
        LiliMessageTemplate messageTemplate = mock(LiliMessageTemplate.class);
        TransactionCommitSendMQListener mqListener = new TransactionCommitSendMQListener();
        ReflectionTestUtils.setField(mqListener, "liliMessageTemplate", messageTemplate);

        List<Runnable> afterCommit = List.of(
                () -> listener.onPaymentCommitted(new DistributionPaymentRewardsEvent(this, "O001")),
                () -> mqListener.send(new TransactionCommitSendMQEvent(
                        this, "order-topic", OrderTagsEnum.STATUS_CHANGE.name(), "{\"orderSn\":\"O001\"}")));

        // 入账在前、MQ 在后：入账失败仍应发出 MQ
        assertDoesNotThrow(() -> invokeAfterCommit(afterCommit));
        verify(facade).handlePaymentRewards("O001");
        verify(messageTemplate).asyncSend(anyString(), anyString(), any(MessageSendCallback.class));
    }

    /**
     * 入账失败发生在 MQ 之后时，MQ 仍已发出。
     */
    @Test
    void facadeFailureAfterMqStillLeavesMqSent() {
        doThrow(new RuntimeException("wallet log column mismatch"))
                .when(facade).handlePaymentRewards("O001");
        LiliMessageTemplate messageTemplate = mock(LiliMessageTemplate.class);
        TransactionCommitSendMQListener mqListener = new TransactionCommitSendMQListener();
        ReflectionTestUtils.setField(mqListener, "liliMessageTemplate", messageTemplate);

        List<Runnable> afterCommit = List.of(
                () -> mqListener.send(new TransactionCommitSendMQEvent(
                        this, "order-topic", OrderTagsEnum.STATUS_CHANGE.name(), "{\"orderSn\":\"O001\"}")),
                () -> listener.onPaymentCommitted(new DistributionPaymentRewardsEvent(this, "O001")));

        // MQ 在前、入账在后：入账失败不影响已发 MQ
        assertDoesNotThrow(() -> invokeAfterCommit(afterCommit));
        verify(messageTemplate, times(1)).asyncSend(anyString(), anyString(), any(MessageSendCallback.class));
        verify(facade).handlePaymentRewards("O001");
    }

    /**
     * 模拟 Spring {@code invokeAfterCommit}：同一阶段监听器顺序执行，前者抛错会打断后者。
     */
    private static void invokeAfterCommit(List<Runnable> listeners) {
        listeners.forEach(Runnable::run);
    }
}
