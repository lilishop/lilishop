package cn.lili.modules.distribution.support;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.DateUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.utils.UuidUtils;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionCustomerRelation;
import cn.lili.modules.distribution.entity.dos.DistributionGrade;
import cn.lili.modules.distribution.entity.dos.DistributionRecruitApplication;
import cn.lili.modules.distribution.entity.dto.DistributionMemberBatchGradeDTO;
import cn.lili.modules.distribution.entity.dto.DistributionMemberCreateDTO;
import cn.lili.modules.distribution.entity.dto.DistributionRecruitApplyFormItemSnapshot;
import cn.lili.modules.distribution.entity.dto.DistributionSearchParams;
import cn.lili.modules.distribution.entity.enums.DistributionRewardTypeEnum;
import cn.lili.modules.distribution.entity.enums.DistributionStatusEnum;
import cn.lili.modules.distribution.entity.vos.DistributionMemberDetailVO;
import cn.lili.modules.distribution.entity.vos.DistributionMemberVO;
import cn.lili.modules.distribution.mapper.DistributionCustomerRelationMapper;
import cn.lili.modules.distribution.mapper.DistributionMapper;
import cn.lili.modules.distribution.service.DistributionCustomerRelationService;
import cn.lili.modules.distribution.service.DistributionGradeService;
import cn.lili.modules.distribution.service.DistributionRecruitService;
import cn.lili.modules.distribution.service.DistributionService;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dto.MemberAddDTO;
import cn.lili.modules.member.service.MemberService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * 分销员管理端维护与查询支持类
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Component
public class DistributionMemberAdminSupport {

    @Autowired
    private MemberService memberService;

    @Autowired
    private DistributionGradeService gradeService;

    @Autowired
    private DistributionRecruitService recruitService;

    @Autowired
    private DistributionCustomerRelationService customerRelationService;

    @Autowired
    private DistributionCustomerRelationMapper customerRelationMapper;

    @Autowired
    private DistributionMemberSupport distributionMemberSupport;

    public DistributionMemberAdminSupport() {
    }

    public DistributionMemberAdminSupport(MemberService memberService,
                                         DistributionGradeService gradeService,
                                         DistributionRecruitService recruitService,
                                         DistributionCustomerRelationService customerRelationService,
                                         DistributionCustomerRelationMapper customerRelationMapper,
                                         DistributionMemberSupport distributionMemberSupport) {
        this.memberService = memberService;
        this.gradeService = gradeService;
        this.recruitService = recruitService;
        this.customerRelationService = customerRelationService;
        this.customerRelationMapper = customerRelationMapper;
        this.distributionMemberSupport = distributionMemberSupport;
    }

    /**
     * 根据搜索条件构建分销员查询包装器
     *
     * @param distributionService      分销服务实例
     * @param distributionSearchParams 搜索参数
     * @return 查询包装器，条件无匹配结果时为空
     */
    public Optional<LambdaQueryWrapper<Distribution>> buildMemberQueryWrapper(DistributionService distributionService,
                                                                             DistributionSearchParams distributionSearchParams) {
        LambdaQueryWrapper<Distribution> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(StringUtils.isNotEmpty(distributionSearchParams.getId()),
                Distribution::getId, distributionSearchParams.getId());
        String nickname = StringUtils.isNotEmpty(distributionSearchParams.getNickname())
                ? distributionSearchParams.getNickname()
                : distributionSearchParams.getMemberName();
        queryWrapper.like(StringUtils.isNotEmpty(nickname),
                Distribution::getMemberName, nickname);
        queryWrapper.eq(StringUtils.isNotEmpty(distributionSearchParams.getDistributionStatus()),
                Distribution::getDistributionStatus, distributionSearchParams.getDistributionStatus());
        if (StringUtils.isNotEmpty(distributionSearchParams.getGradeId())) {
            DistributionGrade grade = gradeService.resolveGrade(distributionSearchParams.getGradeId());
            if (grade == null) {
                return Optional.empty();
            }
            queryWrapper.eq(Distribution::getGradeId, gradeService.formatGradeValue(grade.getSortOrder()));
        }
        Date joinTimeStart = distributionSearchParams.getJoinTimeStart();
        Date joinTimeEnd = distributionSearchParams.getJoinTimeEnd();
        if (joinTimeStart != null && joinTimeEnd != null) {
            queryWrapper.between(Distribution::getJoinTime, joinTimeStart, DateUtil.endOfDate(joinTimeEnd));
        } else if (joinTimeStart != null) {
            queryWrapper.ge(Distribution::getJoinTime, joinTimeStart);
        } else if (joinTimeEnd != null) {
            queryWrapper.le(Distribution::getJoinTime, DateUtil.endOfDate(joinTimeEnd));
        }
        if (StringUtils.isNotEmpty(distributionSearchParams.getMobile())) {
            List<String> memberIds = memberService.list(new LambdaQueryWrapper<Member>()
                            .like(Member::getMobile, distributionSearchParams.getMobile()))
                    .stream()
                    .map(Member::getId)
                    .collect(Collectors.toList());
            if (memberIds.isEmpty()) {
                return Optional.empty();
            }
            queryWrapper.in(Distribution::getMemberId, memberIds);
        }
        if (StringUtils.isNotEmpty(distributionSearchParams.getInviterMobile())) {
            List<String> inviterMemberIds = memberService.list(new LambdaQueryWrapper<Member>()
                            .like(Member::getMobile, distributionSearchParams.getInviterMobile()))
                    .stream()
                    .map(Member::getId)
                    .collect(Collectors.toList());
            if (inviterMemberIds.isEmpty()) {
                return Optional.empty();
            }
            List<String> parentDistributionIds = distributionService.list(new LambdaQueryWrapper<Distribution>()
                            .in(Distribution::getMemberId, inviterMemberIds))
                    .stream()
                    .map(Distribution::getId)
                    .collect(Collectors.toList());
            if (parentDistributionIds.isEmpty()) {
                return Optional.empty();
            }
            queryWrapper.in(Distribution::getParentDistributionId, parentDistributionIds);
        }
        queryWrapper.orderByDesc(Distribution::getCreateTime);
        return Optional.of(queryWrapper);
    }

    /**
     * 获取分销员详情
     *
     * @param distributionService 分销服务实例
     * @param distributionMapper 分销 Mapper 实例
     * @param id                  分销员ID
     * @return 分销员详情 VO
     */
    public DistributionMemberDetailVO getMemberDetail(DistributionService distributionService,
                                                     DistributionMapper distributionMapper,
                                                     String id) {
        Distribution distribution = distributionService.getById(id);
        if (distribution == null) {
            throw new ServiceException(ResultCode.DISTRIBUTION_NOT_EXIST);
        }
        DistributionMemberDetailVO detail = new DistributionMemberDetailVO();
        detail.setId(distribution.getId());
        detail.setMemberId(distribution.getMemberId());
        detail.setJoinTime(distribution.getJoinTime());
        detail.setDistributionStatus(distribution.getDistributionStatus());
        detail.setValidSalesAmount(defaultAmount(distribution.getValidSalesAmount()));
        detail.setOrderCount(distribution.getDistributionOrderCount() == null ? 0 : distribution.getDistributionOrderCount());
        detail.setValidCustomerCount(distribution.getValidCustomerCount() == null ? 0 : distribution.getValidCustomerCount());
        detail.setInviteCount(distribution.getInviteCount() == null ? 0 : distribution.getInviteCount());
        detail.setTotalCommission(defaultAmount(distribution.getValidCommissionAmount()));

        // 累计客户数取客户关系表实时统计
        Long totalCustomers = customerRelationMapper.selectCount(new LambdaQueryWrapper<DistributionCustomerRelation>()
                .eq(DistributionCustomerRelation::getDistributionId, distribution.getId()));
        detail.setTotalCustomerCount(totalCustomers == null ? 0 : totalCustomers.intValue());

        // 按奖励类型拆分商品佣金与邀请奖励
        BigDecimal goodsCommission = distributionMapper.sumRewardByType(
                distribution.getId(), DistributionRewardTypeEnum.DIRECT.name());
        BigDecimal inviteCommission = distributionMapper.sumRewardByType(
                distribution.getId(), DistributionRewardTypeEnum.INVITE.name());
        detail.setGoodsCommission(defaultAmount(goodsCommission));
        detail.setInviteCommission(defaultAmount(inviteCommission));

        // orderType: 0=推广金额, 1=自购金额
        BigDecimal promotionAmount = distributionMapper.sumOrderSales(distribution.getId(), 0);
        BigDecimal selfPurchaseAmount = distributionMapper.sumOrderSales(distribution.getId(), 1);
        detail.setPromotionAmount(defaultAmount(promotionAmount));
        detail.setSelfPurchaseAmount(defaultAmount(selfPurchaseAmount));

        if (memberService != null) {
            Member member = memberService.getById(distribution.getMemberId());
            if (member != null) {
                detail.setMobile(member.getMobile());
                detail.setNickname(member.getNickName());
                detail.setUsername(member.getUsername());
                detail.setFace(member.getFace());
                detail.setSex(member.getSex());
                detail.setMemberCreateTime(member.getCreateTime());
            }
        }
        if (distribution.getGradeId() != null && gradeService != null) {
            DistributionGrade grade = gradeService.resolveGrade(distribution.getGradeId());
            if (grade != null) {
                detail.setGradeName(grade.getGradeName());
            }
        }
        // 待审核时附带招募申请填写信息，供管理端审核弹窗展示
        if (DistributionStatusEnum.APPLY.name().equals(distribution.getDistributionStatus()) && recruitService != null) {
            DistributionRecruitApplication pendingApplication =
                    recruitService.getPendingApplicationByMemberId(distribution.getMemberId());
            if (pendingApplication != null) {
                detail.setPendingApplicationId(pendingApplication.getId());
                detail.setApplyTime(pendingApplication.getCreateTime());
                detail.setApplySource(pendingApplication.getApplySource());
                detail.setApplyFormItems(parseApplyFormItems(pendingApplication.getApplyFormSnapshot()));
            }
        }
        return detail;
    }

    /**
     * 解析申请表单快照 JSON
     *
     * @param applyFormSnapshot 快照 JSON
     * @return 表单项列表；空或非法时返回空列表
     */
    public List<DistributionRecruitApplyFormItemSnapshot> parseApplyFormItems(String applyFormSnapshot) {
        if (CharSequenceUtil.isBlank(applyFormSnapshot)) {
            return Collections.emptyList();
        }
        try {
            List<DistributionRecruitApplyFormItemSnapshot> items =
                    JSONUtil.toList(applyFormSnapshot, DistributionRecruitApplyFormItemSnapshot.class);
            return items == null ? Collections.emptyList() : items;
        } catch (Exception e) {
            return Collections.emptyList();
        }
    }

    /**
     * 后台新增分销员
     *
     * @param distributionService 分销服务实例
     * @param createDTO           新增参数
     * @return 分销员 VO
     */
    public DistributionMemberVO createMember(DistributionService distributionService,
                                            DistributionMemberCreateDTO createDTO) {
        String mobile = createDTO.getMobile() == null ? null : createDTO.getMobile().trim();
        String nickname = createDTO.getNickname() == null ? null : createDTO.getNickname().trim();
        if (StringUtils.isEmpty(mobile) || StringUtils.isEmpty(nickname)) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "昵称和手机号不能为空");
        }
        if (StringUtils.isEmpty(createDTO.getGradeId())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "分销等级不能为空");
        }
        DistributionGrade grade = gradeService.resolveGrade(createDTO.getGradeId());
        if (grade == null) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "分销等级不存在");
        }

        Member member = memberService.findByMobile(mobile);
        // 手机号未注册时自动创建会员账号
        if (member == null) {
            MemberAddDTO memberAddDTO = new MemberAddDTO();
            memberAddDTO.setUsername(mobile);
            memberAddDTO.setMobile(mobile);
            memberAddDTO.setPassword(UuidUtils.getUUID());
            member = memberService.addMember(memberAddDTO);
        }
        member.setNickName(nickname);
        memberService.assignDefaultMemberGradeIfAbsent(member);
        memberService.updateById(member);

        Distribution existing = distributionMemberSupport.getByMemberId(member.getId());
        if (existing != null) {
            String status = existing.getDistributionStatus();
            // 已通过或待审核状态不允许后台重复创建
            if (DistributionStatusEnum.PASS.name().equals(status)) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "该手机号已是分销员");
            }
            if (DistributionStatusEnum.APPLY.name().equals(status)) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "该手机号已有待审核的分销员申请");
            }
        }

        Distribution distribution = distributionMemberSupport.activateOnRecruitPass(member.getId(), nickname);
        distribution.setGradeId(createDTO.getGradeId());
        gradeService.normalizeGradeReference(distribution);
        distributionMemberSupport.updateById(distribution);

        // 指定邀请人时同步建立上下级与客户绑定关系
        if (StringUtils.isNotEmpty(createDTO.getInviterMobile())) {
            String inviterMobile = createDTO.getInviterMobile().trim();
            if (inviterMobile.equals(mobile)) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "邀请人不能是本人");
            }
            String parentDistributionId = resolveDistributionIdByMobile(distributionService, inviterMobile);
            validateInviteCycle(distributionService, distribution.getId(), parentDistributionId);
            distributionService.updateParentByAdmin(distribution.getId(), parentDistributionId);
            String operatorId = UserContext.getCurrentUser() == null ? null : UserContext.getCurrentUser().getId();
            customerRelationService.adminRebind(member.getId(), parentDistributionId, "后台新增分销员", operatorId);
        }

        return toMemberVO(distributionService, distributionMemberSupport.getById(distribution.getId()));
    }

    /**
     * 批量修改分销员等级
     *
     * @param distributionService 分销服务实例
     * @param batchGradeDTO       批量等级参数
     */
    public void batchUpdateGrade(DistributionService distributionService,
                                DistributionMemberBatchGradeDTO batchGradeDTO) {
        if (batchGradeDTO == null || batchGradeDTO.getIds() == null || batchGradeDTO.getIds().isEmpty()) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "请选择分销员");
        }
        if (StringUtils.isEmpty(batchGradeDTO.getGradeId())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "分销等级不能为空");
        }
        DistributionGrade grade = gradeService.resolveGrade(batchGradeDTO.getGradeId());
        if (grade == null) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "分销等级不存在");
        }
        List<String> ids = batchGradeDTO.getIds().stream()
                .filter(StringUtils::isNotEmpty)
                .distinct()
                .collect(Collectors.toList());
        if (ids.isEmpty()) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "请选择分销员");
        }
        List<Distribution> distributions = distributionService.listByIds(ids);
        if (distributions.isEmpty()) {
            throw new ServiceException(ResultCode.DISTRIBUTION_NOT_EXIST);
        }
        for (Distribution distribution : distributions) {
            distribution.setGradeId(batchGradeDTO.getGradeId());
            gradeService.normalizeGradeReference(distribution);
        }
        if (!distributionService.updateBatchById(distributions)) {
            throw new ServiceException(ResultCode.DISTRIBUTION_EDIT_ERROR);
        }
    }

    /**
     * 将分销员实体转换为列表展示 VO
     *
     * @param distributionService 分销服务实例
     * @param distribution        分销员实体
     * @return 分销员 VO
     */
    public DistributionMemberVO toMemberVO(DistributionService distributionService, Distribution distribution) {
        if (distribution == null) {
            return null;
        }
        DistributionMemberVO vo = new DistributionMemberVO();
        vo.setId(distribution.getId());
        vo.setMemberId(distribution.getMemberId());
        vo.setGradeId(distribution.getGradeId());
        vo.setInviteCode(distribution.getInviteCode());
        vo.setValidSalesAmount(distribution.getValidSalesAmount());
        vo.setValidCommissionAmount(distribution.getValidCommissionAmount());
        vo.setValidCustomerCount(distribution.getValidCustomerCount());
        vo.setInviteCount(distribution.getInviteCount());
        vo.setCanRebate(distribution.getCanRebate() == null ? BigDecimal.ZERO : BigDecimal.valueOf(distribution.getCanRebate()));
        vo.setCommissionFrozen(distribution.getCommissionFrozen() == null ? BigDecimal.ZERO : BigDecimal.valueOf(distribution.getCommissionFrozen()));
        vo.setWithdrawalFrozen(distribution.getWithdrawalFrozen());
        vo.setWithdrawnTotal(distribution.getWithdrawnTotal());
        vo.setDistributionStatus(distribution.getDistributionStatus());
        vo.setJoinTime(distribution.getJoinTime());
        if (memberService != null) {
            Member member = memberService.getById(distribution.getMemberId());
            if (member != null) {
                vo.setMobile(member.getMobile());
                vo.setNickname(member.getNickName());
            }
        }
        if (distribution.getGradeId() != null && gradeService != null) {
            DistributionGrade grade = gradeService.resolveGrade(distribution.getGradeId());
            if (grade != null) {
                vo.setGradeName(grade.getGradeName());
            }
        }
        if (StringUtils.isNotEmpty(distribution.getParentDistributionId()) && distributionService != null) {
            vo.setInviterDistributionId(distribution.getParentDistributionId());
            Distribution inviter = distributionService.getById(distribution.getParentDistributionId());
            if (inviter != null) {
                vo.setInviterNickname(inviter.getMemberName());
                if (memberService != null) {
                    Member inviterMember = memberService.getById(inviter.getMemberId());
                    if (inviterMember != null) {
                        vo.setInviterMobile(inviterMember.getMobile());
                    }
                }
            }
        }
        return vo;
    }

    /**
     * 根据手机号解析已通过审核的邀请人分销员ID
     *
     * @param distributionService 分销服务实例
     * @param mobile              邀请人手机号
     * @return 分销员ID
     */
    public String resolveDistributionIdByMobile(DistributionService distributionService, String mobile) {
        Member inviterMember = memberService.getOne(new LambdaQueryWrapper<Member>()
                .eq(Member::getMobile, mobile)
                .last("limit 1"));
        if (inviterMember == null) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "邀请人手机号不正确，请核验后再提交！");
        }
        Distribution inviter = distributionService.getOne(new LambdaQueryWrapper<Distribution>()
                .eq(Distribution::getMemberId, inviterMember.getId())
                .last("limit 1"));
        if (inviter == null || !DistributionStatusEnum.PASS.name().equals(inviter.getDistributionStatus())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "邀请人手机号不正确，请核验后再提交！");
        }
        return inviter.getId();
    }

    /**
     * 校验邀请关系是否形成环（邀请人不能是本人或其下级）
     *
     * @param distributionService  分销服务实例
     * @param childDistributionId  子分销员ID
     * @param parentDistributionId 父分销员ID
     */
    public void validateInviteCycle(DistributionService distributionService, String childDistributionId, String parentDistributionId) {
        if (childDistributionId.equals(parentDistributionId)) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "邀请人不能是本人");
        }
        String current = parentDistributionId;
        Set<String> visited = new HashSet<>();
        while (StringUtils.isNotEmpty(current)) {
            if (childDistributionId.equals(current)) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "邀请人不能为该分销员的下级");
            }
            if (!visited.add(current)) {
                break;
            }
            Distribution parent = distributionService.getById(current);
            current = parent == null ? null : parent.getParentDistributionId();
        }
    }

    public BigDecimal defaultAmount(BigDecimal amount) {
        return amount == null ? BigDecimal.ZERO : amount;
    }
}
