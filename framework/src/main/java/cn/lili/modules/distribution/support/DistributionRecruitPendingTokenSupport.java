package cn.lili.modules.distribution.support;

import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.modules.distribution.serviceimpl.DistributionRecruitServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.concurrent.TimeUnit;

/**
 * 招募邀请 pending token 缓存。关闭线上入口时，支付后自动申请需带回邀请身份。
 *
 * @author Mike
 * @date 2026-09-18
 */
@Component
public class DistributionRecruitPendingTokenSupport {

    /**
     * 与短期绑客同量级，默认 30 天。
     */
    public static final long TTL_DAYS = 30L;

    @Autowired
    private Cache<String> cache;

    /**
     * 为当前会员写入招募邀请 token。
     *
     * @param memberId     会员 ID
     * @param recruitToken 邀请卡 token
     */
    public void save(String memberId, String recruitToken) {
        String token = DistributionRecruitServiceImpl.normalizeRecruitToken(recruitToken);
        if (memberId == null || memberId.isBlank() || token == null) {
            return;
        }
        cache.put(key(memberId), token, TTL_DAYS, TimeUnit.DAYS);
    }

    /**
     * 读取会员待处理招募 token。
     *
     * @param memberId 会员 ID
     * @return 规范化后的分销员 ID；不存在时返回 {@code null}
     */
    public String get(String memberId) {
        if (memberId == null || memberId.isBlank()) {
            return null;
        }
        return cache.get(key(memberId));
    }

    /**
     * 申请成功后清除 pending token。
     *
     * @param memberId 会员 ID
     */
    public void clear(String memberId) {
        if (memberId == null || memberId.isBlank()) {
            return;
        }
        cache.remove(key(memberId));
    }

    private String key(String memberId) {
        return CachePrefix.RECRUIT_PENDING_TOKEN.getPrefix() + memberId;
    }
}
