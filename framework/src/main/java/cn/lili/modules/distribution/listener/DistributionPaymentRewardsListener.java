package cn.lili.modules.distribution.listener;

import cn.lili.modules.distribution.event.DistributionPaymentRewardsEvent;
import cn.lili.modules.distribution.service.DistributionPaymentRewardsFacade;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.event.TransactionPhase;
import org.springframework.transaction.event.TransactionalEventListener;

/**
 * 支付事务提交后入账分销佣金。
 * <p>
 * 本监听器不加 {@code @Transactional}，方法体只 try-catch，失败只打日志、绝不外抛，
 * 以免打断同阶段的订单状态 MQ。真正的写库事务在
 * {@link DistributionPaymentRewardsFacade} 上以 {@code REQUIRES_NEW} 开启。
 * </p>
 *
 * @author Mike
 * @date 2026-09-07
 */
@Slf4j
@Component
public class DistributionPaymentRewardsListener {

    /**
     * 独立事务入账门面。
     */
    @Autowired
    private DistributionPaymentRewardsFacade distributionPaymentRewardsFacade;

    /**
     * 支付事务提交后再入账；失败只记录日志。
     *
     * @param event 仅含订单号的提交后事件
     */
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    public void onPaymentCommitted(DistributionPaymentRewardsEvent event) {
        try {
            // 独立事务入账；异常由下方 catch 吞掉，避免打断同阶段订单状态 MQ
            distributionPaymentRewardsFacade.handlePaymentRewards(event.getOrderSn());
        } catch (Exception e) {
            log.error("订单[{}]分销奖励入账失败", event.getOrderSn(), e);
        }
    }
}
