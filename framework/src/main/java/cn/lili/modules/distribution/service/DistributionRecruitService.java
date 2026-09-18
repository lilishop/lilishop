package cn.lili.modules.distribution.service;

import cn.lili.modules.distribution.entity.dos.DistributionRecruitApplication;
import cn.lili.modules.distribution.entity.dto.DistributionRecruitApplyDTO;
import cn.lili.modules.distribution.entity.vos.DistributionRecruitConditionProgressVO;
import cn.lili.modules.distribution.entity.vos.DistributionRecruitInviterVO;
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
     *
     * @param setting 招募方案配置
     * @return 保存后的招募设置
     */
    DistributionRecruitSetting saveSetting(DistributionRecruitSetting setting);

    /**
     * 获取当前招募设置。
     *
     * @return 当前生效的招募设置
     */
    DistributionRecruitSetting getSetting();

    /**
     * 买家端获取招募页展示数据。
     *
     * @return 招募页方案、表单字段及当前会员申请状态（已登录时）
     */
    DistributionRecruitPageVO getRecruitPage();

    /**
     * 根据邀请分销员 ID 获取招募页邀请人展示信息（无需登录）。
     *
     * @param distributionId 邀请人分销员 ID
     * @return 邀请人昵称、头像等展示信息；分销员不存在或未通过审核时返回空对象
     */
    DistributionRecruitInviterVO getRecruitInviter(String distributionId);

    /**
     * 会员提交分销员招募申请。
     *
     * @param applyDTO 申请参数（含招募令牌、协议确认、自定义表单等）
     * @return 新建或更新后的招募申请记录
     */
    DistributionRecruitApplication submitApplication(DistributionRecruitApplyDTO applyDTO);

    /**
     * 审核招募申请。
     *
     * @param applicationId 招募申请 ID
     * @param pass          是否通过
     * @param remark        审核备注
     * @param operatorId    操作人 ID
     */
    void auditApplication(String applicationId, boolean pass, String remark, String operatorId);

    /**
     * 查询会员当前待审核的招募申请。
     *
     * @param memberId 会员 ID
     * @return 待审核申请；不存在时返回 {@code null}
     */
    DistributionRecruitApplication getPendingApplicationByMemberId(String memberId);

    /**
     * 将历史待审核招募申请同步到分销员待审核列表。
     */
    void syncPendingApplyDistributions();

    /**
     * 满足自动申请条件时尝试代会员提交申请。
     * <p>
     * 预期业务拒绝（已申请、入口关闭、token 无效等）以 {@link cn.lili.common.exception.ServiceException} 抛出；
     * 调用方（支付监听器、招募页接口）应捕获，避免影响主流程。
     *
     * @param memberId 会员 ID
     */
    void tryAutoApply(String memberId);

    /**
     * 登录会员上报招募邀请 token，供关闭线上入口时自动申请使用。
     *
     * @param memberId     当前会员 ID
     * @param recruitToken 邀请卡 token
     */
    void savePendingRecruitToken(String memberId, String recruitToken);

    /**
     * 查询会员招募准入条件完成进度。
     *
     * @param memberId 会员 ID；未登录场景可传 {@code null}，此时各项进度视为未达标
     * @return 自购金额、消费笔数、指定商品等条件的完成进度
     */
    DistributionRecruitConditionProgressVO getConditionProgress(String memberId);
}
