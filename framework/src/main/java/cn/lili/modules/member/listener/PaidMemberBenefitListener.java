package cn.lili.modules.member.listener;

import cn.lili.modules.member.event.PaidMemberActivatedEvent;
import cn.lili.modules.member.service.MemberBenefitService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.transaction.event.TransactionPhase;
import org.springframework.transaction.event.TransactionalEventListener;

/**
 * 付费会员支付提交后的权益发放监听器，解除支付资格服务对权益发放服务的反向依赖。
 * 发放事务由逐项发放服务独立开启，失败记录日志并由周期任务恢复，不撤销已支付资格。
 *
 * @author Bulbasaur
 * @since 2026-11-05
 */
@Slf4j
@Component
public class PaidMemberBenefitListener {
    /** 权益发放入口，按订单重新核验资格及快照。 */
    private final MemberBenefitService benefits;

    /**
     * 注入权益发放入口，不向支付服务注入监听器。
     * @param benefits 客户权益服务
     */
    public PaidMemberBenefitListener(MemberBenefitService benefits) { this.benefits = benefits; }

    /**
     * 仅在支付事务成功提交后执行；无事务事件或回滚事件不触发，也不提前发放未来续费区间。
     * @param event 首次到账的会员订单事件
     */
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    public void onActivated(PaidMemberActivatedEvent event) {
        try {
            benefits.deliverPaidMemberBenefits(event.getOrderId());
        } catch (Exception e) {
            log.error("付费会员订单[{}]提交后权益发放失败，周期任务将重试", event.getOrderId(), e);
        }
    }
}
