package cn.lili.modules.distribution.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

/**
 * 分销支付入账门面：在独立事务中执行冻结佣金，与支付事务解耦。
 * <p>
 * {@link Propagation#REQUIRES_NEW} 必须落在本 Bean 上。
 * 吞异常只能写在监听器里，二者不能写在同一方法，否则 afterCommit 再抛
 * {@code UnexpectedRollbackException} 仍会打断同阶段的订单状态 MQ。
 * </p>
 *
 * @author Mike
 * @date 2026-09-07
 */
@Service
public class DistributionPaymentRewardsFacade {

    /**
     * 分销奖励服务（写单、冻结、流水）。
     */
    @Autowired
    private DistributionRewardService distributionRewardService;

    /**
     * 在新事务中写入分销订单并冻结佣金。
     *
     * @param orderSn 订单编号
     */
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    public void handlePaymentRewards(String orderSn) {
        // REQUIRES_NEW：写分销单并冻结佣金，与支付事务解耦
        distributionRewardService.handlePaymentRewards(orderSn);
    }
}
