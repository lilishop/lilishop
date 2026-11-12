package cn.lili.modules.distribution.support;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dto.DistributionInviteeQueryDTO;
import cn.lili.modules.distribution.entity.vos.DistributionInviteCardVO;
import cn.lili.modules.distribution.entity.vos.DistributionInvitePageVO;
import cn.lili.modules.distribution.entity.vos.DistributionPromotionPosterVO;
import cn.lili.modules.distribution.mapper.DistributionInviteMapper;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.system.entity.dto.DistributionInviteCardSetting;
import cn.lili.modules.system.entity.dto.DistributionPromotionPosterSetting;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;

/**
 * 分销员推广海报、邀请卡与下级邀请列表支持类
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Component
public class DistributionPromotionSupport {

    @Autowired
    private DistributionPromotionPosterSettingSupport distributionPromotionPosterSettingSupport;

    @Autowired
    private DistributionInviteCardSettingSupport distributionInviteCardSettingSupport;

    @Autowired
    private DistributionInviteMapper distributionInviteMapper;

    @Autowired
    private MemberService memberService;

    public DistributionPromotionSupport() {
    }

    public DistributionPromotionSupport(DistributionPromotionPosterSettingSupport distributionPromotionPosterSettingSupport,
                                      DistributionInviteCardSettingSupport distributionInviteCardSettingSupport,
                                      DistributionInviteMapper distributionInviteMapper,
                                      MemberService memberService) {
        this.distributionPromotionPosterSettingSupport = distributionPromotionPosterSettingSupport;
        this.distributionInviteCardSettingSupport = distributionInviteCardSettingSupport;
        this.distributionInviteMapper = distributionInviteMapper;
        this.memberService = memberService;
    }

    /**
     * 获取分销员推广海报信息
     *
     * @param distribution 分销员实体
     * @return 推广海报 VO
     */
    public DistributionPromotionPosterVO getPromotionPoster(Distribution distribution) {
        DistributionPromotionPosterSetting setting = distributionPromotionPosterSettingSupport.readSetting();
        DistributionPromotionPosterVO vo = new DistributionPromotionPosterVO();
        vo.setDistributionId(distribution.getId());
        vo.setMemberName(distribution.getMemberName());
        if (memberService != null) {
            Member member = memberService.getById(distribution.getMemberId());
            if (member != null) {
                if (CharSequenceUtil.isNotBlank(member.getNickName())) {
                    vo.setMemberName(member.getNickName());
                }
                vo.setMemberAvatar(member.getFace());
            }
        }
        vo.setMemberInfoVisible(setting.getMemberInfoVisible());
        vo.setBackgroundImage(setting.getBackgroundImage());
        vo.setSlogan(setting.getSlogan());
        vo.setTextColor(setting.getTextColor());
        vo.setSharePage("pages/tabbar/home/index");
        vo.setShareScene("bind," + distribution.getId());
        return vo;
    }

    /**
     * 获取分销员邀请卡信息
     *
     * @param distribution 分销员实体
     * @return 邀请卡 VO
     */
    public DistributionInviteCardVO getInviteCard(Distribution distribution) {
        DistributionInviteCardSetting setting = distributionInviteCardSettingSupport.readSetting();
        DistributionInviteCardVO vo = new DistributionInviteCardVO();
        vo.setDistributionId(distribution.getId());
        vo.setInviteCode(distribution.getInviteCode());
        vo.setMemberName(distribution.getMemberName());
        if (memberService != null) {
            Member member = memberService.getById(distribution.getMemberId());
            if (member != null) {
                if (CharSequenceUtil.isNotBlank(member.getNickName())) {
                    vo.setMemberName(member.getNickName());
                }
                vo.setMemberAvatar(member.getFace());
            }
        }
        vo.setMemberInfoVisible(setting.getMemberInfoVisible());
        vo.setBackgroundImage(setting.getBackgroundImage());
        vo.setSlogan(setting.getSlogan());
        vo.setTextColor(setting.getTextColor());
        vo.setSharePage("pages/mine/distribution/join");
        vo.setShareScene("recruit_" + distribution.getId());
        return vo;
    }

    /**
     * 分页查询下级邀请列表
     *
     * @param parentDistributionId 上级分销员ID
     * @param query                查询条件
     * @return 邀请列表分页 VO
     */
    public DistributionInvitePageVO pageInvitees(String parentDistributionId, DistributionInviteeQueryDTO query) {
        DistributionInviteeQueryDTO safeQuery = query == null ? new DistributionInviteeQueryDTO() : query;
        long current = safeQuery.getPageNumber() == null || safeQuery.getPageNumber() <= 0 ? 1 : safeQuery.getPageNumber();
        long size = safeQuery.getPageSize() == null || safeQuery.getPageSize() <= 0 ? 10 : safeQuery.getPageSize();
        DistributionInviteRangeSupport.DateRange range = DistributionInviteRangeSupport.resolveRange(safeQuery);

        long total = distributionInviteMapper.countInvitees(parentDistributionId, range.getStartTime(), range.getEndTime());
        DistributionInvitePageVO pageVO = new DistributionInvitePageVO();
        pageVO.setTotal(total);
        pageVO.setCurrent(current);
        pageVO.setSize(size);
        // 汇总时间范围内的邀请奖励总额
        BigDecimal totalReward = distributionInviteMapper.sumInviteReward(
                parentDistributionId, range.getStartTime(), range.getEndTime());
        pageVO.setTotalInviteReward(totalReward == null ? BigDecimal.ZERO : totalReward);
        if (total <= 0) {
            return pageVO;
        }
        long offset = (current - 1) * size;
        pageVO.setRecords(distributionInviteMapper.listInvitees(
                parentDistributionId, range.getStartTime(), range.getEndTime(), offset, size));
        return pageVO;
    }
}
