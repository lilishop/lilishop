package cn.lili.modules.distribution.serviceimpl;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * 招募邀请令牌规范化单元测试
 */
class DistributionRecruitTokenTest {

    /**
     * 验证招募令牌兼容原始分销员ID。
     */
    @Test
    void normalizeRecruitToken_acceptsRawDistributionId() {
        assertEquals("dist-1", DistributionRecruitServiceImpl.normalizeRecruitToken("dist-1"));
    }

    /**
     * 验证招募令牌去除招募前缀及首尾空白。
     */
    @Test
    void normalizeRecruitToken_stripsRecruitPrefixAndWhitespace() {
        assertEquals("dist-1", DistributionRecruitServiceImpl.normalizeRecruitToken("  recruit_dist-1  "));
    }

    /**
     * 验证空招募令牌规范化后返回空值。
     */
    @Test
    void normalizeRecruitToken_returnsNullWhenBlank() {
        assertNull(DistributionRecruitServiceImpl.normalizeRecruitToken(null));
        assertNull(DistributionRecruitServiceImpl.normalizeRecruitToken("   "));
        assertNull(DistributionRecruitServiceImpl.normalizeRecruitToken("recruit_"));
    }
}
