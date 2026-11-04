package cn.lili.modules.distribution.support;

import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.enums.DistributionStatusEnum;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 招募申请拦截：仅拒绝与无记录可申请，清退不可再申请。
 *
 * @author Mike
 * @date 2026-09-09
 */
class DistributionMemberSupportRecruitApplyTest {

    private final DistributionMemberSupport support = new DistributionMemberSupport();

    /**
     * 验证无记录及审核拒绝状态允许提交招募申请。
     */
    @Test
    void isBlockingRecruitApply_allowsNullAndRefuse() {
        assertThat(support.isBlockingRecruitApply(null)).isFalse();
        assertThat(support.isBlockingRecruitApply(status(DistributionStatusEnum.REFUSE))).isFalse();
    }

    /**
     * 验证已通过、申请中及已退出状态阻止重复招募申请。
     */
    @Test
    void isBlockingRecruitApply_blocksPassApplyAndRetreat() {
        assertThat(support.isBlockingRecruitApply(status(DistributionStatusEnum.PASS))).isTrue();
        assertThat(support.isBlockingRecruitApply(status(DistributionStatusEnum.APPLY))).isTrue();
        assertThat(support.isBlockingRecruitApply(status(DistributionStatusEnum.RETREAT))).isTrue();
    }

    /**
     * 构造指定分销状态的测试记录。
     *
     * @param status 测试状态
     * @return 指定分销状态的测试记录
     */
    private static Distribution status(DistributionStatusEnum status) {
        Distribution distribution = new Distribution();
        distribution.setDistributionStatus(status.name());
        return distribution;
    }
}
