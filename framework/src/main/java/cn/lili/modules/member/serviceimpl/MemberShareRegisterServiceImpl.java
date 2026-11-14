package cn.lili.modules.member.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.RandomUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dos.MemberShareBuyLog;
import cn.lili.modules.member.entity.dos.MemberShareCode;
import cn.lili.modules.member.entity.dos.MemberShareRegisterLog;
import cn.lili.modules.member.entity.enums.ExperienceRuleEnum;
import cn.lili.modules.member.entity.vo.MemberShareBuyLogVO;
import cn.lili.modules.member.mapper.MemberExperienceLogMapper;
import cn.lili.modules.member.mapper.MemberMapper;
import cn.lili.modules.member.mapper.MemberShareBuyLogMapper;
import cn.lili.modules.member.mapper.MemberShareCodeMapper;
import cn.lili.modules.member.mapper.MemberShareRegisterLogMapper;
import cn.lili.modules.member.service.MemberExperienceService;
import cn.lili.modules.member.service.MemberShareRegisterService;
import cn.lili.modules.member.support.MemberShareSourceSupport;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import cn.lili.modules.order.order.entity.dos.Order;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;

/**
 * 会员分享注册奖励服务实现
 */
@Service
public class MemberShareRegisterServiceImpl implements MemberShareRegisterService {

    @Autowired
    private MemberShareSourceSupport memberShareSourceSupport;

    private static final String OPEN_STATE = "OPEN";
    private static final String SUCCESS_STATUS = "SUCCESS";
    private static final String SKIPPED_STATUS = "SKIPPED";

    @Autowired
    private MemberShareCodeMapper memberShareCodeMapper;
    @Autowired
    private MemberShareRegisterLogMapper memberShareRegisterLogMapper;
    @Autowired
    private MemberShareBuyLogMapper memberShareBuyLogMapper;
    @Autowired
    private MemberExperienceLogMapper memberExperienceLogMapper;
    @Autowired
    private MemberExperienceService memberExperienceService;
    @Autowired
    private Cache cache;
    @Autowired
    private MemberMapper memberMapper;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public String getOrCreateRegisterShareCode(String memberId) {
        MemberShareCode existed = memberShareCodeMapper.findByMemberId(memberId);
        if (existed != null && CharSequenceUtil.isNotBlank(existed.getShareCode())) {
            return existed.getShareCode();
        }
        String shareCode = generateUniqueShareCode();
        MemberShareCode memberShareCode = new MemberShareCode();
        memberShareCode.setMemberId(memberId);
        memberShareCode.setShareCode(shareCode);
        memberShareCode.setState(OPEN_STATE);
        memberShareCodeMapper.insert(memberShareCode);
        return shareCode;
    }

    @Override
    public void bindRegisterSource(String inviteeId, String shareCode) {
        memberShareSourceSupport.bindRegisterSource(inviteeId, shareCode);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void handleShareRegisterReward(Member invitee) {
        if (invitee == null || CharSequenceUtil.isBlank(invitee.getId())) {
            return;
        }
        String inviteeId = invitee.getId();
        String inviterId = (String) cache.get(CachePrefix.INVITER.getPrefix() + inviteeId);
        String shareCode = (String) cache.get(CachePrefix.INVITER_SHARE_CODE.getPrefix() + inviteeId);

        try {
            if (CharSequenceUtil.isBlank(inviterId) || CharSequenceUtil.equals(inviterId, inviteeId)) {
                return;
            }
            Member inviter = memberMapper.selectById(inviterId);
            if (inviter == null) {
                return;
            }
            Long existed = memberShareRegisterLogMapper.countByInviteeId(inviteeId);
            if (existed != null && existed > 0) {
                return;
            }

            memberExperienceService.grantExperience(inviterId, ExperienceRuleEnum.SHARE_REGISTER, inviteeId, "邀请注册成功，赠送经验值");
            Long rewardExperience = memberExperienceLogMapper.getVariableByMemberRuleAndBiz(inviterId, ExperienceRuleEnum.SHARE_REGISTER.name(), inviteeId);

            MemberShareRegisterLog registerLog = new MemberShareRegisterLog();
            registerLog.setInviterId(inviterId);
            registerLog.setInviteeId(inviteeId);
            registerLog.setInviteeMobile(invitee.getMobile());
            registerLog.setShareCode(shareCode);
            registerLog.setRewardExperience(rewardExperience == null ? 0L : rewardExperience);
            registerLog.setRewardStatus((rewardExperience != null && rewardExperience > 0) ? SUCCESS_STATUS : SKIPPED_STATUS);
            memberShareRegisterLogMapper.insert(registerLog);
        } finally {
            cache.remove(CachePrefix.INVITER.getPrefix() + inviteeId);
            cache.remove(CachePrefix.INVITER_SHARE_CODE.getPrefix() + inviteeId);
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void handleShareBuyReward(Order order) {
        if (order == null || CharSequenceUtil.isBlank(order.getSn()) || CharSequenceUtil.isBlank(order.getMemberId())) {
            return;
        }
        Long existed = memberShareBuyLogMapper.countByOrderSn(order.getSn());
        if (existed != null && existed > 0) {
            return;
        }
        String inviteeId = order.getMemberId();
        String inviterId = memberShareRegisterLogMapper.findInviterIdByInviteeId(inviteeId);
        if (CharSequenceUtil.isBlank(inviterId) || CharSequenceUtil.equals(inviterId, inviteeId)) {
            return;
        }
        memberExperienceService.grantExperience(inviterId, ExperienceRuleEnum.SHARE_BUY, order.getSn(), "邀请用户支付订单，赠送经验值");
        Long rewardExperience = memberExperienceLogMapper.getVariableByMemberRuleAndBiz(inviterId, ExperienceRuleEnum.SHARE_BUY.name(), order.getSn());

        MemberShareBuyLog buyLog = new MemberShareBuyLog();
        buyLog.setInviterId(inviterId);
        buyLog.setInviteeId(inviteeId);
        buyLog.setOrderSn(order.getSn());
        buyLog.setOrderAmount(order.getFlowPrice());
        buyLog.setRewardExperience(rewardExperience == null ? 0L : rewardExperience);
        buyLog.setRewardStatus((rewardExperience != null && rewardExperience > 0) ? SUCCESS_STATUS : SKIPPED_STATUS);
        memberShareBuyLogMapper.insert(buyLog);
    }

    @Override
    public IPage<MemberShareBuyLogVO> getShareBuyLogPage(String mobile, Date startTime, Date endTime, PageVO page) {
        QueryWrapper<MemberShareBuyLog> wrapper = new QueryWrapper<>();
        wrapper.and(CharSequenceUtil.isNotBlank(mobile),
                w -> w.eq("inviter.mobile", mobile).or().eq("invitee.mobile", mobile));
        wrapper.ge(startTime != null, "sb.create_time", startTime);
        wrapper.le(endTime != null, "sb.create_time", endTime);
        wrapper.orderByDesc("sb.create_time");
        return memberShareBuyLogMapper.pageWithMember(PageUtil.initPage(page), wrapper);
    }

    private String generateUniqueShareCode() {
        for (int i = 0; i < 10; i++) {
            String shareCode = RandomUtil.randomStringUpper(8);
            MemberShareCode existed = memberShareCodeMapper.findByShareCode(shareCode);
            if (existed == null) {
                return shareCode;
            }
        }
        return RandomUtil.randomStringUpper(10);
    }
}
