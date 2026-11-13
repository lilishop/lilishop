package cn.lili.modules.distribution.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionCustomerRelation;
import cn.lili.modules.distribution.entity.dos.DistributionGrade;
import cn.lili.modules.distribution.entity.dos.DistributionRecruitApplication;
import cn.lili.modules.distribution.entity.dto.DistributionInviteeQueryDTO;
import cn.lili.modules.distribution.entity.dto.DistributionMemberBatchGradeDTO;
import cn.lili.modules.distribution.entity.dto.DistributionMemberCreateDTO;
import cn.lili.modules.distribution.entity.dto.DistributionMemberUpdateDTO;
import cn.lili.modules.distribution.entity.dto.DistributionSearchParams;
import cn.lili.modules.distribution.entity.enums.DistributionStatusEnum;
import cn.lili.modules.distribution.entity.vos.DistributionInviteCardVO;
import cn.lili.modules.distribution.entity.vos.DistributionInvitePageVO;
import cn.lili.modules.distribution.entity.vos.DistributionMemberDetailVO;
import cn.lili.modules.distribution.entity.vos.DistributionMemberVO;
import cn.lili.modules.distribution.entity.vos.DistributionPromotionPosterVO;
import cn.lili.modules.distribution.mapper.DistributionMapper;
import cn.lili.modules.distribution.service.DistributionCustomerRelationService;
import cn.lili.modules.distribution.service.DistributionGradeService;
import cn.lili.modules.distribution.service.DistributionRecruitService;
import cn.lili.modules.distribution.service.DistributionService;
import cn.lili.modules.distribution.support.DistributionMemberAdminSupport;
import cn.lili.modules.distribution.support.DistributionMemberExportSupport;
import cn.lili.modules.distribution.support.DistributionMemberSupport;
import cn.lili.modules.distribution.support.DistributionParentBindSupport;
import cn.lili.modules.distribution.support.DistributionPromotionSupport;
import cn.lili.modules.distribution.support.DistributionSettingSupport;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.system.entity.dto.DistributionSetting;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

/**
 * 分销员接口实现，管理维护、导出及推广组件均为必需依赖，不在业务入口手动创建支持类
 *
 * @author pikachu
 * @since 2020-03-14 23:04:56
 */
@Service
public class DistributionServiceImpl extends ServiceImpl<DistributionMapper, Distribution> implements DistributionService {

    @Autowired
    private MemberService memberService;
    @Autowired
    private DistributionCustomerRelationService customerRelationService;
    @Autowired
    private DistributionRecruitService recruitService;
    @Autowired
    private DistributionGradeService gradeService;
    @Autowired
    private DistributionMemberSupport distributionMemberSupport;
    @Autowired
    private DistributionSettingSupport distributionSettingSupport;
    @Autowired
    private DistributionParentBindSupport distributionParentBindSupport;
    @Autowired
    private DistributionMemberAdminSupport distributionMemberAdminSupport;
    @Autowired
    private DistributionMemberExportSupport distributionMemberExportSupport;
    @Autowired
    private DistributionPromotionSupport distributionPromotionSupport;

    @Override
    public IPage<Distribution> distributionPage(DistributionSearchParams distributionSearchParams, PageVO page) {
        return distributionMemberAdminSupport.buildMemberQueryWrapper(this, distributionSearchParams)
                .map(queryWrapper -> this.page(PageUtil.initPage(page), queryWrapper))
                .orElseGet(() -> new Page<>(page.getPageNumber(), page.getPageSize(), 0));
    }

    @Override
    public Distribution getDistribution() {
        return this.getOne(new LambdaQueryWrapper<Distribution>().eq(Distribution::getMemberId, UserContext.getCurrentUser().getId()));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean audit(String id, String status) {
        // 检查分销开关
        checkDistributionSetting();

        // 根据id获取分销员
        Distribution distribution = this.getById(id);
        if (Optional.ofNullable(distribution).isPresent()) {
            // 存在招募申请时走招募审核流程，与旧版直接审核互斥
            DistributionRecruitApplication pendingApplication =
                    recruitService.getPendingApplicationByMemberId(distribution.getMemberId());
            if (pendingApplication != null) {
                boolean pass = DistributionStatusEnum.PASS.name().equals(status);
                String operatorId = UserContext.getCurrentUser() == null
                        ? null : UserContext.getCurrentUser().getId();
                String remark = pass ? "审核通过" : "审核拒绝";
                recruitService.auditApplication(pendingApplication.getId(), pass, remark, operatorId);
                return true;
            }
            if (status.equals(DistributionStatusEnum.PASS.name())) {
                distribution.setDistributionStatus(DistributionStatusEnum.PASS.name());
                distribution.setJoinTime(new Date());
                if (distribution.getInviteCode() == null) {
                    distribution.setInviteCode(distributionMemberSupport.generateInviteCode());
                }
                gradeService.assignBaseGradeIfAbsent(distribution);
                // 审核通过时按配置绑定上级分销员
                bindParentOnAuditPass(distribution.getId(), null,
                        getBoundDistributorId(distribution.getMemberId()));
            } else {
                distribution.setDistributionStatus(DistributionStatusEnum.REFUSE.name());
            }
            return this.updateById(distribution);
        }
        return false;
    }

    @Override
    public boolean retreat(String id) {
        // 检查分销开关
        checkDistributionSetting();

        // 根据id获取分销员
        Distribution distribution = this.getById(id);
        if (Optional.ofNullable(distribution).isPresent()) {
            distribution.setDistributionStatus(DistributionStatusEnum.RETREAT.name());
            return this.updateById(distribution);
        }
        return false;
    }

    @Override
    public boolean resume(String id) {
        // 检查分销开关
        checkDistributionSetting();

        // 根据id获取分销员
        Distribution distribution = this.getById(id);
        if (Optional.ofNullable(distribution).isPresent()) {
            distribution.setDistributionStatus(DistributionStatusEnum.PASS.name());
            return this.updateById(distribution);
        }
        return false;
    }

    @Override
    public void bindingDistribution(String distributionId) {
        // 判断用户是否登录，未登录不能进行绑定
        if (UserContext.getCurrentUser() == null) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        String memberId = UserContext.getCurrentUser().getId();
        customerRelationService.bindOnScan(distributionId, memberId, "SCAN");
    }

    @Override
    public void checkDistributionSetting() {
        DistributionSetting distributionSetting = distributionSettingSupport.getSetting();
        if (Boolean.FALSE.equals(distributionSetting.getIsOpen())) {
            throw new ServiceException(ResultCode.DISTRIBUTION_CLOSE);
        }
    }

    @Override
    public void subRebate(Double canRebate, String distributionId, Double distributionOrderPrice) {
        this.baseMapper.subRebate(canRebate, distributionId, distributionOrderPrice);
    }

    @Override
    public void addRebate(Double rebate, String distributionId, Double distributionOrderPrice) {
        this.baseMapper.addRebate(rebate, distributionId, distributionOrderPrice);
    }

    @Override
    public void addCanRebate(Double rebate, String distributionId) {
        this.baseMapper.addCanRebate(rebate, distributionId);
    }

    @Override
    public void addCashRebate(Double rebate, String distributionId) {
        this.baseMapper.addCashRebate(rebate, distributionId);
    }

    @Override
    public void subCashRebate(Double rebate, String distributionId) {
        this.baseMapper.subCashRebate(rebate, distributionId);
    }

    @Override
    public boolean isRetreated(String memberId) {
        Distribution distribution = this.getOne(new LambdaQueryWrapper<Distribution>()
                .eq(Distribution::getMemberId, memberId));
        return distribution != null
                && DistributionStatusEnum.RETREAT.name().equals(distribution.getDistributionStatus());
    }

    @Override
    public IPage<DistributionMemberVO> getMemberPage(DistributionSearchParams distributionSearchParams, PageVO page) {
        if (DistributionStatusEnum.APPLY.name().equals(distributionSearchParams.getDistributionStatus())) {
            recruitService.syncPendingApplyDistributions();
        }
        IPage<Distribution> distributionPage = distributionPage(distributionSearchParams, page);
        Page<DistributionMemberVO> result = new Page<>(distributionPage.getCurrent(), distributionPage.getSize(), distributionPage.getTotal());
        result.setRecords(distributionPage.getRecords().stream().map(this::toMemberVO).toList());
        return result;
    }

    @Override
    public DistributionMemberDetailVO getMemberDetail(String id) {
        return distributionMemberAdminSupport.getMemberDetail(this, this.baseMapper, id);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public DistributionMemberVO updateMember(String id, DistributionMemberUpdateDTO updateDTO) {
        Distribution distribution = this.getById(id);
        if (distribution == null) {
            throw new ServiceException(ResultCode.DISTRIBUTION_NOT_EXIST);
        }
        String nickname = updateDTO.getNickname() == null ? null : updateDTO.getNickname().trim();
        if (StringUtils.isEmpty(nickname)) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "昵称不能为空");
        }
        Member member = memberService != null ? memberService.getById(distribution.getMemberId()) : null;
        if (member != null) {
            member.setNickName(nickname);
            memberService.updateById(member);
        }
        distribution.setMemberName(nickname);

        if (StringUtils.isNotEmpty(updateDTO.getGradeId())) {
            DistributionGrade grade = gradeService.resolveGrade(updateDTO.getGradeId());
            if (grade == null) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "分销等级不存在");
            }
            distribution.setGradeId(updateDTO.getGradeId());
            gradeService.normalizeGradeReference(distribution);
        }

        if (updateDTO.getInviterMobile() != null) {
            String inviterMobile = updateDTO.getInviterMobile().trim();
            String operatorId = UserContext.getCurrentUser() == null ? null : UserContext.getCurrentUser().getId();
            if (StringUtils.isEmpty(inviterMobile)) {
                String oldParentId = distribution.getParentDistributionId();
                updateParentByAdmin(id, null);
                distribution.setParentDistributionId(null);
                if (member != null && StringUtils.isNotEmpty(oldParentId) && customerRelationService != null) {
                    DistributionCustomerRelation customerRelation = customerRelationService.getActiveRelation(member.getId());
                    if (customerRelation != null && oldParentId.equals(customerRelation.getDistributionId())) {
                        customerRelationService.adminUnbind(member.getId(), "后台编辑分销员清除邀请人", operatorId);
                    }
                }
            } else {
                if (member != null && inviterMobile.equals(member.getMobile())) {
                    throw new ServiceException(ResultCode.PARAMS_ERROR, "邀请人不能是本人");
                }
                String parentDistributionId = distributionMemberAdminSupport.resolveDistributionIdByMobile(this, inviterMobile);
                distributionMemberAdminSupport.validateInviteCycle(this, id, parentDistributionId);
                updateParentByAdmin(id, parentDistributionId);
                distribution.setParentDistributionId(parentDistributionId);
                if (member != null && customerRelationService != null) {
                    customerRelationService.adminRebind(member.getId(), parentDistributionId, "后台编辑分销员", operatorId);
                }
            }
        }

        if (!this.updateById(distribution)) {
            throw new ServiceException(ResultCode.DISTRIBUTION_EDIT_ERROR);
        }
        return toMemberVO(this.getById(id));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public DistributionMemberVO createMember(DistributionMemberCreateDTO createDTO) {
        return distributionMemberAdminSupport.createMember(this, createDTO);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void batchUpdateGrade(DistributionMemberBatchGradeDTO batchGradeDTO) {
        distributionMemberAdminSupport.batchUpdateGrade(this, batchGradeDTO);
    }

    @Override
    public void exportMembers(HttpServletResponse response, DistributionSearchParams distributionSearchParams) {
        if (DistributionStatusEnum.APPLY.name().equals(distributionSearchParams.getDistributionStatus())) {
            recruitService.syncPendingApplyDistributions();
        }
        List<Distribution> distributions = distributionMemberAdminSupport.buildMemberQueryWrapper(this, distributionSearchParams)
                .map(this::list)
                .orElse(Collections.emptyList());
        distributionMemberExportSupport.exportMembers(this, this.baseMapper, response, distributions);
    }

    @Override
    public DistributionPromotionPosterVO getPromotionPoster() {
        checkDistributionSetting();
        Distribution distribution = getDistribution();
        if (distribution == null || isRetreated(distribution.getMemberId())) {
            throw new ServiceException(ResultCode.DISTRIBUTION_NOT_EXIST);
        }
        return distributionPromotionSupport.getPromotionPoster(distribution);
    }

    @Override
    public DistributionInviteCardVO getInviteCard() {
        checkDistributionSetting();
        Distribution distribution = getDistribution();
        if (distribution == null || isRetreated(distribution.getMemberId())) {
            throw new ServiceException(ResultCode.DISTRIBUTION_NOT_EXIST);
        }
        return distributionPromotionSupport.getInviteCard(distribution);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void bindParentOnAuditPass(String childDistributionId, String recruitInviterId, String boundDistributorId) {
        distributionParentBindSupport.bindParentOnAuditPass(childDistributionId, recruitInviterId, boundDistributorId);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateParentByAdmin(String childDistributionId, String parentDistributionId) {
        Distribution child = getById(childDistributionId);
        if (child == null) {
            return;
        }
        String oldParentId = child.getParentDistributionId();
        if (parentDistributionId == null) {
            if (StringUtils.isEmpty(oldParentId)) {
                return;
            }
            this.update(new LambdaUpdateWrapper<Distribution>()
                    .eq(Distribution::getId, childDistributionId)
                    .set(Distribution::getParentDistributionId, null));
            syncInviteCountAndGrade(oldParentId);
            return;
        }
        if (Objects.equals(oldParentId, parentDistributionId)) {
            return;
        }
        child.setParentDistributionId(parentDistributionId);
        updateById(child);
        syncInviteCountAndGrade(oldParentId);
        syncInviteCountAndGrade(parentDistributionId);
    }

    @Override
    public DistributionInvitePageVO pageInvitees(String parentDistributionId, DistributionInviteeQueryDTO query) {
        return distributionPromotionSupport.pageInvitees(parentDistributionId, query);
    }

    private DistributionMemberVO toMemberVO(Distribution distribution) {
        return distributionMemberAdminSupport.toMemberVO(this, distribution);
    }

    private String getBoundDistributorId(String memberId) {
        DistributionCustomerRelation relation = customerRelationService.getActiveRelation(memberId);
        return relation == null ? null : relation.getDistributionId();
    }

    private void syncInviteCountAndGrade(String distributionId) {
        distributionParentBindSupport.syncInviteCountAndGrade(distributionId);
    }

}
