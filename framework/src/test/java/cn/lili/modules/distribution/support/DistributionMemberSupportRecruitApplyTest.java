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

    @Test
    void isBlockingRecruitApply_allowsNullAndRefuse() {
        assertThat(support.isBlockingRecruitApply(null)).isFalse();
        assertThat(support.isBlockingRecruitApply(status(DistributionStatusEnum.REFUSE))).isFalse();
    }

    @Test
    void isBlockingRecruitApply_blocksPassApplyAndRetreat() {
        assertThat(support.isBlockingRecruitApply(status(DistributionStatusEnum.PASS))).isTrue();
        assertThat(support.isBlockingRecruitApply(status(DistributionStatusEnum.APPLY))).isTrue();
        assertThat(support.isBlockingRecruitApply(status(DistributionStatusEnum.RETREAT))).isTrue();
    }

    private static Distribution status(DistributionStatusEnum status) {
        Distribution distribution = new Distribution();
        distribution.setDistributionStatus(status.name());
        return distribution;
    }
}
