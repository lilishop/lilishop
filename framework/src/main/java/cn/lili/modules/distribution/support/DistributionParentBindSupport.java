package cn.lili.modules.distribution.support;

import cn.lili.common.utils.StringUtils;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.enums.DistributionParentAttributionModeEnum;
import cn.lili.modules.distribution.mapper.DistributionMapper;
import cn.lili.modules.distribution.service.DistributionGradeService;
import cn.lili.modules.system.entity.dto.DistributionSetting;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 分销员上级归属绑定辅助，避免 RecruitService 与 DistributionService 循环依赖。
 *
 * @author Bulbasaur
 * @since 2026-08-05
 */
@Component
public class DistributionParentBindSupport {

    @Autowired
    private DistributionMapper distributionMapper;

    @Autowired
    private DistributionSettingSupport distributionSettingSupport;

    @Autowired
    private DistributionGradeService gradeService;

    /**
     * 审核通过时按全局配置绑定上级分销员。
     *
     * @param childDistributionId  下级分销员 ID
     * @param recruitInviterId     招募邀请人分销员 ID
     * @param boundDistributorId   客户绑定分销员 ID
     */
    public void bindParentOnAuditPass(String childDistributionId, String recruitInviterId, String boundDistributorId) {
        Distribution child = distributionMapper.selectById(childDistributionId);
        if (child == null || StringUtils.isNotEmpty(child.getParentDistributionId())) {
            return;
        }
        DistributionSetting distributionSetting = distributionSettingSupport.getSetting();
        String parentAttributionMode = distributionSetting.getParentAttributionMode() == null
                ? DistributionParentAttributionModeEnum.RECRUIT_INVITER.name()
                : distributionSetting.getParentAttributionMode();
        String parentDistributionId;
        // 按全局配置决定上级取招募邀请人还是客户绑定分销员
        if (DistributionParentAttributionModeEnum.BOUND_DISTRIBUTOR.name().equals(parentAttributionMode)) {
            parentDistributionId = boundDistributorId;
        } else {
            parentDistributionId = recruitInviterId;
        }
        if (StringUtils.isEmpty(parentDistributionId)) {
            return;
        }
        child.setParentDistributionId(parentDistributionId);
        distributionMapper.updateById(child);
        syncInviteCountAndGrade(parentDistributionId);
    }

    /**
     * 同步分销员邀请人数并刷新等级。
     *
     * @param distributionId 分销员 ID
     */
    public void syncInviteCountAndGrade(String distributionId) {
        if (StringUtils.isEmpty(distributionId)) {
            return;
        }
        distributionMapper.syncInviteCount(distributionId);
        gradeService.refreshGrade(distributionId);
    }
}
