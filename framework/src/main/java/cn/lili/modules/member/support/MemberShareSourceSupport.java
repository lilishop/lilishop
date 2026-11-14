package cn.lili.modules.member.support;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.modules.member.entity.dos.MemberShareCode;
import cn.lili.modules.member.mapper.MemberShareCodeMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

/**
 * 注册邀请来源绑定能力，仅查询分享码并缓存邀请关系，不依赖会员注册或经验奖励服务。
 * 沿用原有规则：空参数、无效分享码及自邀跳过；奖励发放仍由后续奖励流程处理。
 *
 * @author Bulbasaur
 * @since 2026-11-14
 */
@Component
@RequiredArgsConstructor
public class MemberShareSourceSupport {

    private final MemberShareCodeMapper shareCodes;
    private final Cache cache;

    /**
     * 记录有效分享码对应的邀请人及分享码，供后续注册奖励流程读取。
     * 不新增数据库事务或重试机制，保留原有缓存覆盖及有效期规则。
     *
     * @param inviteeId 被邀请会员ID
     * @param shareCode 注册分享码
     */
    public void bindRegisterSource(String inviteeId, String shareCode) {
        if (CharSequenceUtil.isBlank(inviteeId) || CharSequenceUtil.isBlank(shareCode)) {
            return;
        }
        MemberShareCode source = shareCodes.findByShareCode(shareCode);
        if (source == null || CharSequenceUtil.isBlank(source.getMemberId())
                || CharSequenceUtil.equals(inviteeId, source.getMemberId())) {
            return;
        }
        cache.put(CachePrefix.INVITER.getPrefix() + inviteeId, source.getMemberId());
        cache.put(CachePrefix.INVITER_SHARE_CODE.getPrefix() + inviteeId, shareCode);
    }
}
