package cn.lili.modules.distribution.serviceimpl;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * 招募邀请令牌规范化单元测试
 */
class DistributionRecruitTokenTest {

    @Test
    void normalizeRecruitToken_acceptsRawDistributionId() {
        assertEquals("dist-1", DistributionRecruitServiceImpl.normalizeRecruitToken("dist-1"));
    }

    @Test
    void normalizeRecruitToken_stripsRecruitPrefixAndWhitespace() {
        assertEquals("dist-1", DistributionRecruitServiceImpl.normalizeRecruitToken("  recruit_dist-1  "));
    }

    @Test
    void normalizeRecruitToken_returnsNullWhenBlank() {
        assertNull(DistributionRecruitServiceImpl.normalizeRecruitToken(null));
        assertNull(DistributionRecruitServiceImpl.normalizeRecruitToken("   "));
        assertNull(DistributionRecruitServiceImpl.normalizeRecruitToken("recruit_"));
    }
}
