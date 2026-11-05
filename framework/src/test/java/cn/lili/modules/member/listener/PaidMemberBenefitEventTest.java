package cn.lili.modules.member.listener;

import cn.lili.modules.member.event.PaidMemberActivatedEvent;
import cn.lili.modules.member.service.MemberBenefitService;
import org.junit.jupiter.api.Test;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.transaction.TransactionDefinition;
import org.springframework.transaction.event.TransactionalEventListenerFactory;
import org.springframework.transaction.support.AbstractPlatformTransactionManager;
import org.springframework.transaction.support.DefaultTransactionStatus;
import org.springframework.transaction.support.TransactionTemplate;
import static org.assertj.core.api.Assertions.*;
import static org.mockito.Mockito.*;

/**
 * 使用真实Spring事务同步生命周期验证付费会员权益仅在提交后发放，回滚及失败不会误发或撤销支付。
 * 不连接数据库，不能替代真实支付事务和多连接并发验收。
 * @author Bulbasaur
 * @since 2026-11-05
 */
class PaidMemberBenefitEventTest {
    /**
     * 注册真实事务事件工厂及监听器，使用模拟发放入口隔离外部业务。
     * @param benefits 模拟权益发放服务
     * @return 由调用方关闭的Spring容器
     */
    private AnnotationConfigApplicationContext context(MemberBenefitService benefits) {
        var context = new AnnotationConfigApplicationContext();
        context.registerBean(TransactionalEventListenerFactory.class);
        context.registerBean(MemberBenefitService.class, () -> benefits);
        context.register(PaidMemberBenefitListener.class);
        context.refresh();
        return context;
    }

    /** 支付未提交时不调用发放，成功提交后才传递订单主键。 */
    @Test void deliversOnlyAfterSuccessfulCommit() {
        var benefits = mock(MemberBenefitService.class);
        try (var context = context(benefits)) {
            var manager = new TestTransactionManager();
            new TransactionTemplate(manager).executeWithoutResult(tx -> {
                context.publishEvent(new PaidMemberActivatedEvent(this,"order"));
                verifyNoInteractions(benefits);
            });
            verify(benefits).deliverPaidMemberBenefits("order");
            assertThat(manager.commits).isEqualTo(1);
        }
    }

    /** 支付事务回滚后不发放，禁止提前兑现尚未持久化的会员资格。 */
    @Test void rollbackNeverDeliversBenefits() {
        var benefits = mock(MemberBenefitService.class);
        try (var context = context(benefits)) {
            new TransactionTemplate(new TestTransactionManager()).executeWithoutResult(tx -> {
                context.publishEvent(new PaidMemberActivatedEvent(this,"order"));
                tx.setRollbackOnly();
            });
            verifyNoInteractions(benefits);
        }
    }

    /** 无事务事件没有提交凭据，保持默认不执行fallback发放。 */
    @Test void eventWithoutTransactionDoesNotDeliver() {
        var benefits = mock(MemberBenefitService.class);
        try (var context = context(benefits)) {
            context.publishEvent(new PaidMemberActivatedEvent(this,"order"));
            verifyNoInteractions(benefits);
        }
    }

    /** 发放失败只记录日志，支付事务仍成功提交，后续由既有周期扫描恢复。 */
    @Test void failedDeliveryDoesNotTurnSuccessfulPaymentIntoFailure() {
        var benefits = mock(MemberBenefitService.class);
        doThrow(new IllegalStateException("发券暂时失败")).when(benefits).deliverPaidMemberBenefits("order");
        try (var context = context(benefits)) {
            var manager = new TestTransactionManager();
            assertThatCode(() -> new TransactionTemplate(manager).executeWithoutResult(tx ->
                    context.publishEvent(new PaidMemberActivatedEvent(this,"order")))).doesNotThrowAnyException();
            verify(benefits).deliverPaidMemberBenefits("order");
            assertThat(manager.commits).isEqualTo(1);
        }
    }

    /**
     * 无数据库资源的事务管理器，仅驱动Spring提交/回滚同步回调。
     * @author Bulbasaur
     * @since 2026-11-05
     */
    private static class TestTransactionManager extends AbstractPlatformTransactionManager {
        /** 成功提交次数，不表示实际数据库提交次数。 */
        private int commits;
        /** @return 用于事务同步的空资源标识 */
        @Override protected Object doGetTransaction() { return new Object(); }
        /**
         * 初始化测试事务，无需打开数据库连接。
         * @param transaction 测试资源
         * @param definition 事务定义
         */
        @Override protected void doBegin(Object transaction, TransactionDefinition definition) { }
        /**
         * 记录提交次数。
         * @param status 事务状态
         */
        @Override protected void doCommit(DefaultTransactionStatus status) { commits++; }
        /**
         * 回滚测试事务，不操作数据库。
         * @param status 事务状态
         */
        @Override protected void doRollback(DefaultTransactionStatus status) { }
    }
}
