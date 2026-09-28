package cn.lili.modules.distribution.listener;

import cn.lili.modules.distribution.event.DistributionRecruitAutoApplyEvent;
import cn.lili.modules.distribution.service.DistributionRecruitAutoApplyFacade;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.event.TransactionPhase;
import org.springframework.transaction.event.TransactionalEventListener;

/**
 * 支付事务提交后尝试分销招募自动申请。
 * <p>
 * 本监听器不加 {@code @Transactional}，失败只打日志、绝不外抛，
 * 以免打断同阶段的订单状态 MQ。写库事务在
 * {@link DistributionRecruitAutoApplyFacade} 上以 {@code REQUIRES_NEW} 开启。
 * </p>
 *
 * @author Mike
 * @date 2026-09-18
 */
@Slf4j
@Component
public class DistributionRecruitAutoApplyListener {

    @Autowired
    private DistributionRecruitAutoApplyFacade distributionRecruitAutoApplyFacade;

    /**
     * 支付提交后尝试自动申请。
     *
     * @param event 含会员 ID 的提交后事件
     */
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    public void onPaymentCommitted(DistributionRecruitAutoApplyEvent event) {
        try {
            distributionRecruitAutoApplyFacade.tryAutoApply(event.getMemberId());
        } catch (Exception e) {
            log.error("会员[{}]分销招募自动申请失败", event.getMemberId(), e);
        }
    }
}
