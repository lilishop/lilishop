package cn.lili.modules.distribution.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

/**
 * 分销招募自动申请门面：在独立事务中执行，与支付事务解耦。
 *
 * @author Mike
 * @date 2026-09-18
 */
@Service
public class DistributionRecruitAutoApplyFacade {

    @Autowired
    private DistributionRecruitService distributionRecruitService;

    /**
     * 在新事务中尝试自动申请。
     *
     * @param memberId 会员 ID
     */
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    public void tryAutoApply(String memberId) {
        distributionRecruitService.tryAutoApply(memberId);
    }
}
