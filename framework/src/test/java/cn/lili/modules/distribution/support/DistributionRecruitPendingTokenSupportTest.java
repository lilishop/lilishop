package cn.lili.modules.distribution.support;

import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.concurrent.TimeUnit;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 招募 pending token 缓存读写。
 *
 * @author Mike
 * @date 2026-09-18
 */
class DistributionRecruitPendingTokenSupportTest {

    private DistributionRecruitPendingTokenSupport support;
    private Cache<String> cache;

    @BeforeEach
    void setUp() {
        support = new DistributionRecruitPendingTokenSupport();
        cache = mock(Cache.class);
        ReflectionTestUtils.setField(support, "cache", cache);
    }

    /**
     * 保存时去掉 recruit_ 前缀并写入带 TTL 的缓存。
     */
    @Test
    void save_normalizesAndPuts() {
        support.save("M1", "recruit_D1");
        verify(cache).put(CachePrefix.RECRUIT_PENDING_TOKEN.getPrefix() + "M1",
                "D1", DistributionRecruitPendingTokenSupport.TTL_DAYS, TimeUnit.DAYS);
    }

    /**
     * 按会员读取缓存中的 pending token。
     */
    @Test
    void get_returnsCachedToken() {
        when(cache.get(CachePrefix.RECRUIT_PENDING_TOKEN.getPrefix() + "M1")).thenReturn("D1");
        assertThat(support.get("M1")).isEqualTo("D1");
    }

    /**
     * 清除该会员的 pending token。
     */
    @Test
    void clear_removesKey() {
        support.clear("M1");
        verify(cache).remove(CachePrefix.RECRUIT_PENDING_TOKEN.getPrefix() + "M1");
    }
}
