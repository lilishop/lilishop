package cn.lili.modules.distribution.support;

import cn.lili.common.utils.StringUtils;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.enums.DistributionParentAttributionModeEnum;
import cn.lili.modules.distribution.mapper.DistributionMapper;
import cn.lili.modules.distribution.service.DistributionGradeService;
import cn.lili.modules.system.entity.dto.DistributionSetting;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.HashSet;
import java.util.Set;

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
        if (StringUtils.isEmpty(parentDistributionId) || isInviteCycle(childDistributionId, parentDistributionId)) {
            return;
        }
        child.setParentDistributionId(parentDistributionId);
        distributionMapper.updateById(child);
        syncInviteCountAndGrade(parentDistributionId);
    }

    /**
     * 判断挂上级是否会形成自邀或邀请环。成环时跳过绑定，不阻断审核。
     *
     * @param childDistributionId  下级分销员 ID
     * @param parentDistributionId 拟绑定的上级分销员 ID
     * @return 是否成环
     */
    private boolean isInviteCycle(String childDistributionId, String parentDistributionId) {
        if (childDistributionId.equals(parentDistributionId)) {
            return true;
        }
        String current = parentDistributionId;
        Set<String> visited = new HashSet<>();
        while (StringUtils.isNotEmpty(current)) {
            if (childDistributionId.equals(current)) {
                return true;
            }
            if (!visited.add(current)) {
                break;
            }
            Distribution parent = distributionMapper.selectById(current);
            current = parent == null ? null : parent.getParentDistributionId();
        }
        return false;
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
