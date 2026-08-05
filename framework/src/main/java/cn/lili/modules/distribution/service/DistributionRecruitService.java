package cn.lili.modules.distribution.service;

import cn.lili.modules.distribution.entity.dos.DistributionRecruitApplication;
import cn.lili.modules.distribution.entity.dto.DistributionRecruitApplyDTO;
import cn.lili.modules.distribution.entity.vos.DistributionRecruitConditionProgressVO;
import cn.lili.modules.distribution.entity.vos.DistributionRecruitPageVO;
import cn.lili.modules.system.entity.dto.DistributionRecruitSetting;

/**
 * 分销招募服务。
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
public interface DistributionRecruitService {

    /**
     * 保存招募设置。
     */
    DistributionRecruitSetting saveSetting(DistributionRecruitSetting setting);

    /**
     * 获取当前招募设置。
     */
    DistributionRecruitSetting getSetting();

    /**
     * 买家端获取招募页展示数据。
     */
    DistributionRecruitPageVO getRecruitPage();

    /**
     * 会员提交分销员招募申请。
     */
    DistributionRecruitApplication submitApplication(DistributionRecruitApplyDTO applyDTO);

    /**
     * 审核招募申请。
     */
    void auditApplication(String applicationId, boolean pass, String remark, String operatorId);

    /**
     * 查询会员当前待审核的招募申请。
     */
    DistributionRecruitApplication getPendingApplicationByMemberId(String memberId);

    /**
     * 将历史待审核招募申请同步到分销员待审核列表。
     */
    void syncPendingApplyDistributions();

    /**
     * 满足自动申请条件时尝试代会员提交申请。
     */
    void tryAutoApply(String memberId);

    /**
     * 查询会员招募准入条件完成进度。
     */
    DistributionRecruitConditionProgressVO getConditionProgress(String memberId);
}
