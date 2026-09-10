package cn.lili.modules.distribution.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.BeanUtil;
import cn.lili.common.utils.DateUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.utils.UuidUtils;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.connect.entity.Connect;
import cn.lili.modules.connect.entity.enums.SourceEnum;
import cn.lili.modules.connect.service.ConnectService;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionCustomerRelation;
import cn.lili.modules.distribution.entity.dos.DistributionGrade;
import cn.lili.modules.distribution.entity.dos.DistributionRecruitApplication;
import cn.lili.modules.distribution.entity.dto.DistributionInviteeQueryDTO;
import cn.lili.modules.distribution.entity.dto.DistributionMemberBatchGradeDTO;
import cn.lili.modules.distribution.entity.dto.DistributionMemberCreateDTO;
import cn.lili.modules.distribution.entity.dto.DistributionMemberUpdateDTO;
import cn.lili.modules.distribution.entity.dto.DistributionRecruitApplyFormItemSnapshot;
import cn.lili.modules.distribution.entity.dto.DistributionSearchParams;
import cn.lili.modules.distribution.entity.enums.DistributionRewardTypeEnum;
import cn.lili.modules.distribution.entity.enums.DistributionStatusEnum;
import cn.lili.modules.distribution.entity.vos.DistributionMemberDetailVO;
import cn.lili.modules.distribution.entity.vos.DistributionMemberVO;
import cn.lili.modules.distribution.entity.vos.DistributionInviteCardVO;
import cn.lili.modules.distribution.entity.vos.DistributionInvitePageVO;
import cn.lili.modules.distribution.entity.vos.DistributionPromotionPosterVO;
import cn.lili.modules.distribution.mapper.DistributionCustomerRelationMapper;
import cn.lili.modules.distribution.mapper.DistributionInviteMapper;
import cn.lili.modules.distribution.mapper.DistributionMapper;
import cn.lili.modules.distribution.service.DistributionCustomerRelationService;
import cn.lili.modules.distribution.service.DistributionGradeService;
import cn.lili.modules.distribution.service.DistributionRecruitService;
import cn.lili.modules.distribution.service.DistributionService;
import cn.lili.modules.distribution.support.DistributionInviteCardSettingSupport;
import cn.lili.modules.distribution.support.DistributionInviteRangeSupport;
import cn.lili.modules.distribution.support.DistributionPromotionPosterSettingSupport;
import cn.lili.modules.distribution.support.DistributionMemberSupport;
import cn.lili.modules.distribution.support.DistributionParentBindSupport;
import cn.lili.modules.distribution.support.DistributionSettingSupport;
import cn.lili.modules.finance.export.FinanceExportHelper;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dto.MemberAddDTO;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.system.entity.dto.DistributionInviteCardSetting;
import cn.lili.modules.system.entity.dto.DistributionPromotionPosterSetting;
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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * 分销员接口实现
 *
 * @author pikachu
 * @since 2020-03-14 23:04:56
 */
@Service
public class DistributionServiceImpl extends ServiceImpl<DistributionMapper, Distribution> implements DistributionService {

    /**
     * 客户
     */
    @Autowired
    private MemberService memberService;

    @Autowired
    private DistributionCustomerRelationService customerRelationService;

    @Autowired
    private DistributionRecruitService recruitService;

    @Autowired
    private DistributionInviteMapper distributionInviteMapper;

    @Autowired
    private DistributionGradeService gradeService;

    @Autowired
    private DistributionMemberSupport distributionMemberSupport;

    @Autowired
    private DistributionCustomerRelationMapper customerRelationMapper;

    @Autowired
    private DistributionSettingSupport distributionSettingSupport;

    @Autowired
    private DistributionPromotionPosterSettingSupport distributionPromotionPosterSettingSupport;

    @Autowired
    private DistributionInviteCardSettingSupport distributionInviteCardSettingSupport;

    @Autowired
    private DistributionParentBindSupport distributionParentBindSupport;

    @Autowired
    private ConnectService connectService;

    @Override
    public IPage<Distribution> distributionPage(DistributionSearchParams distributionSearchParams, PageVO page) {
        return buildMemberQueryWrapper(distributionSearchParams)
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

        //检查分销开关
        checkDistributionSetting();

        //根据id获取分销员
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

        //检查分销开关
        checkDistributionSetting();

        //根据id获取分销员
        Distribution distribution = this.getById(id);
        if (Optional.ofNullable(distribution).isPresent()) {
            distribution.setDistributionStatus(DistributionStatusEnum.RETREAT.name());
            return this.updateById(distribution);
        }
        return false;
    }

    @Override
    public boolean resume(String id) {

        //检查分销开关
        checkDistributionSetting();

        //根据id获取分销员
        Distribution distribution = this.getById(id);
        if (Optional.ofNullable(distribution).isPresent()) {
            distribution.setDistributionStatus(DistributionStatusEnum.PASS.name());
            return this.updateById(distribution);
        }
        return false;
    }

    @Override
    public void bindingDistribution(String distributionId) {

        //判断用户是否登录，未登录不能进行绑定
        if (UserContext.getCurrentUser() == null) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        String memberId = UserContext.getCurrentUser().getId();
        customerRelationService.bindOnScan(distributionId, memberId, "SCAN");
    }

    /**
     * 检查分销设置开关
     */
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
        Distribution distribution = this.getById(id);
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
        BigDecimal goodsCommission = this.baseMapper.sumRewardByType(
                distribution.getId(), DistributionRewardTypeEnum.DIRECT.name());
        BigDecimal inviteCommission = this.baseMapper.sumRewardByType(
                distribution.getId(), DistributionRewardTypeEnum.INVITE.name());
        detail.setGoodsCommission(defaultAmount(goodsCommission));
        detail.setInviteCommission(defaultAmount(inviteCommission));

        // orderType: 0=推广金额, 1=自购金额
        BigDecimal promotionAmount = this.baseMapper.sumOrderSales(distribution.getId(), 0);
        BigDecimal selfPurchaseAmount = this.baseMapper.sumOrderSales(distribution.getId(), 1);
        detail.setPromotionAmount(defaultAmount(promotionAmount));
        detail.setSelfPurchaseAmount(defaultAmount(selfPurchaseAmount));

        Member member = memberService.getById(distribution.getMemberId());
        if (member != null) {
            detail.setMobile(member.getMobile());
            detail.setNickname(member.getNickName());
            detail.setUsername(member.getUsername());
            detail.setFace(member.getFace());
            detail.setSex(member.getSex());
            detail.setMemberCreateTime(member.getCreateTime());
        }
        if (distribution.getGradeId() != null) {
            DistributionGrade grade = gradeService.resolveGrade(distribution.getGradeId());
            if (grade != null) {
                detail.setGradeName(grade.getGradeName());
            }
        }
        // 待审核时附带招募申请填写信息，供管理端审核弹窗展示
        if (DistributionStatusEnum.APPLY.name().equals(distribution.getDistributionStatus())) {
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
     * 解析申请表单快照 JSON。
     *
     * @param applyFormSnapshot 快照 JSON
     * @return 表单项列表；空或非法时返回空列表
     */
    private List<DistributionRecruitApplyFormItemSnapshot> parseApplyFormItems(String applyFormSnapshot) {
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
        Member member = memberService.getById(distribution.getMemberId());
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
                if (member != null && StringUtils.isNotEmpty(oldParentId)) {
                    DistributionCustomerRelation customerRelation = customerRelationService.getActiveRelation(member.getId());
                    if (customerRelation != null && oldParentId.equals(customerRelation.getDistributionId())) {
                        customerRelationService.adminUnbind(member.getId(), "后台编辑分销员清除邀请人", operatorId);
                    }
                }
            } else {
                if (member != null && inviterMobile.equals(member.getMobile())) {
                    throw new ServiceException(ResultCode.PARAMS_ERROR, "邀请人不能是本人");
                }
                String parentDistributionId = resolveDistributionIdByMobile(inviterMobile);
                validateInviteCycle(id, parentDistributionId);
                updateParentByAdmin(id, parentDistributionId);
                distribution.setParentDistributionId(parentDistributionId);
                if (member != null) {
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
            String parentDistributionId = resolveDistributionIdByMobile(inviterMobile);
            validateInviteCycle(distribution.getId(), parentDistributionId);
            updateParentByAdmin(distribution.getId(), parentDistributionId);
            String operatorId = UserContext.getCurrentUser() == null ? null : UserContext.getCurrentUser().getId();
            customerRelationService.adminRebind(member.getId(), parentDistributionId, "后台新增分销员", operatorId);
        }

        return toMemberVO(distributionMemberSupport.getById(distribution.getId()));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void batchUpdateGrade(DistributionMemberBatchGradeDTO batchGradeDTO) {
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
        List<Distribution> distributions = this.listByIds(ids);
        if (distributions.isEmpty()) {
            throw new ServiceException(ResultCode.DISTRIBUTION_NOT_EXIST);
        }
        for (Distribution distribution : distributions) {
            distribution.setGradeId(batchGradeDTO.getGradeId());
            gradeService.normalizeGradeReference(distribution);
        }
        if (!this.updateBatchById(distributions)) {
            throw new ServiceException(ResultCode.DISTRIBUTION_EDIT_ERROR);
        }
    }

    @Override
    public void exportMembers(HttpServletResponse response, DistributionSearchParams distributionSearchParams) {
        if (DistributionStatusEnum.APPLY.name().equals(distributionSearchParams.getDistributionStatus())) {
            recruitService.syncPendingApplyDistributions();
        }
        List<Distribution> distributions = buildMemberQueryWrapper(distributionSearchParams)
                .map(this::list)
                .orElse(Collections.emptyList());
        FinanceExportHelper.checkRowLimit(distributions.size());

        // 批量预加载会员、openid 及邀请人信息，避免逐行查库
        Set<String> memberIds = distributions.stream()
                .map(Distribution::getMemberId)
                .filter(StringUtils::isNotEmpty)
                .collect(Collectors.toSet());
        Map<String, Member> memberMap = memberIds.isEmpty() ? Collections.emptyMap()
                : memberService.listByIds(memberIds).stream()
                .collect(Collectors.toMap(Member::getId, item -> item, (a, b) -> a));
        Map<String, String> openIdMap = loadOpenIdMap(memberIds);

        Set<String> inviterDistributionIds = distributions.stream()
                .map(Distribution::getParentDistributionId)
                .filter(StringUtils::isNotEmpty)
                .collect(Collectors.toSet());
        Map<String, Distribution> inviterMap = inviterDistributionIds.isEmpty() ? Collections.emptyMap()
                : this.listByIds(inviterDistributionIds).stream()
                .collect(Collectors.toMap(Distribution::getId, item -> item, (a, b) -> a));
        Set<String> inviterMemberIds = inviterMap.values().stream()
                .map(Distribution::getMemberId)
                .filter(StringUtils::isNotEmpty)
                .collect(Collectors.toSet());
        Map<String, Member> inviterMemberMap = inviterMemberIds.isEmpty() ? Collections.emptyMap()
                : memberService.listByIds(inviterMemberIds).stream()
                .collect(Collectors.toMap(Member::getId, item -> item, (a, b) -> a));
        Map<String, String> inviterOpenIdMap = loadOpenIdMap(inviterMemberIds);

        List<String> headers = List.of(
                "手机号", "分销员openid", "昵称", "等级", "邀请方", "邀请方分销员openid", "加入时间",
                "累计客户", "有效客户", "累计邀请", "累计收益", "累计成交金额", "累计首单金额",
                "累计推广订单数", "累计推广金额", "商品佣金", "邀请奖励");
        List<List<Object>> rows = new ArrayList<>();
        for (Distribution distribution : distributions) {
            Member member = memberMap.get(distribution.getMemberId());
            String mobile = member == null ? "" : Objects.toString(member.getMobile(), "");
            String nickname = member == null ? Objects.toString(distribution.getMemberName(), "")
                    : Objects.toString(member.getNickName(), "");
            String openId = openIdMap.getOrDefault(distribution.getMemberId(), "");
            String gradeName = "";
            if (distribution.getGradeId() != null) {
                DistributionGrade grade = gradeService.resolveGrade(distribution.getGradeId());
                if (grade != null) {
                    gradeName = Objects.toString(grade.getGradeName(), "");
                }
            }
            String inviterName = "";
            String inviterOpenId = "";
            if (StringUtils.isNotEmpty(distribution.getParentDistributionId())) {
                Distribution inviter = inviterMap.get(distribution.getParentDistributionId());
                if (inviter != null) {
                    Member inviterMember = inviterMemberMap.get(inviter.getMemberId());
                    inviterName = inviterMember == null
                            ? Objects.toString(inviter.getMemberName(), "")
                            : Objects.toString(inviterMember.getNickName(), inviter.getMemberName());
                    inviterOpenId = inviterOpenIdMap.getOrDefault(inviter.getMemberId(), "");
                }
            }
            Long totalCustomers = customerRelationMapper.selectCount(new LambdaQueryWrapper<DistributionCustomerRelation>()
                    .eq(DistributionCustomerRelation::getDistributionId, distribution.getId()));
            BigDecimal goodsCommission = defaultAmount(this.baseMapper.sumRewardByType(
                    distribution.getId(), DistributionRewardTypeEnum.DIRECT.name()));
            BigDecimal inviteCommission = defaultAmount(this.baseMapper.sumRewardByType(
                    distribution.getId(), DistributionRewardTypeEnum.INVITE.name()));
            BigDecimal promotionAmount = defaultAmount(this.baseMapper.sumOrderSales(distribution.getId(), 0));
            BigDecimal firstOrderAmount = defaultAmount(this.baseMapper.sumFirstOrderAmount(distribution.getId()));
            Long promotionOrderCount = this.baseMapper.countPromotionOrders(distribution.getId());

            rows.add(List.of(
                    mobile,
                    openId,
                    nickname,
                    gradeName,
                    inviterName == null ? "" : inviterName,
                    inviterOpenId,
                    distribution.getJoinTime() == null ? ""
                            : cn.hutool.core.date.DateUtil.formatDateTime(distribution.getJoinTime()),
                    totalCustomers == null ? 0 : totalCustomers,
                    distribution.getValidCustomerCount() == null ? 0 : distribution.getValidCustomerCount(),
                    distribution.getInviteCount() == null ? 0 : distribution.getInviteCount(),
                    defaultAmount(distribution.getValidCommissionAmount()),
                    defaultAmount(distribution.getValidSalesAmount()),
                    firstOrderAmount,
                    promotionOrderCount == null ? 0 : promotionOrderCount,
                    promotionAmount,
                    goodsCommission,
                    inviteCommission
            ));
        }

        FinanceExportHelper.writeExcel(response, "分销员列表", writer ->
                FinanceExportHelper.writeSheet(writer, "分销员列表", headers, rows));
    }

    @Override
    public DistributionPromotionPosterVO getPromotionPoster() {
        checkDistributionSetting();
        Distribution distribution = getDistribution();
        if (distribution == null || isRetreated(distribution.getMemberId())) {
            throw new ServiceException(ResultCode.DISTRIBUTION_NOT_EXIST);
        }
        DistributionPromotionPosterSetting setting = distributionPromotionPosterSettingSupport.readSetting();
        DistributionPromotionPosterVO vo = new DistributionPromotionPosterVO();
        vo.setDistributionId(distribution.getId());
        vo.setMemberName(distribution.getMemberName());
        Member member = memberService.getById(distribution.getMemberId());
        if (member != null) {
            if (CharSequenceUtil.isNotBlank(member.getNickName())) {
                vo.setMemberName(member.getNickName());
            }
            vo.setMemberAvatar(member.getFace());
        }
        vo.setMemberInfoVisible(setting.getMemberInfoVisible());
        vo.setBackgroundImage(setting.getBackgroundImage());
        vo.setSlogan(setting.getSlogan());
        vo.setTextColor(setting.getTextColor());
        vo.setSharePage("pages/tabbar/home/index");
        vo.setShareScene("bind," + distribution.getId());
        return vo;
    }

    @Override
    public DistributionInviteCardVO getInviteCard() {
        checkDistributionSetting();
        Distribution distribution = getDistribution();
        if (distribution == null || isRetreated(distribution.getMemberId())) {
            throw new ServiceException(ResultCode.DISTRIBUTION_NOT_EXIST);
        }
        DistributionInviteCardSetting setting = distributionInviteCardSettingSupport.readSetting();
        DistributionInviteCardVO vo = new DistributionInviteCardVO();
        vo.setDistributionId(distribution.getId());
        vo.setInviteCode(distribution.getInviteCode());
        vo.setMemberName(distribution.getMemberName());
        Member member = memberService.getById(distribution.getMemberId());
        if (member != null) {
            if (CharSequenceUtil.isNotBlank(member.getNickName())) {
                vo.setMemberName(member.getNickName());
            }
            vo.setMemberAvatar(member.getFace());
        }
        vo.setMemberInfoVisible(setting.getMemberInfoVisible());
        vo.setBackgroundImage(setting.getBackgroundImage());
        vo.setSlogan(setting.getSlogan());
        vo.setTextColor(setting.getTextColor());
        vo.setSharePage("pages/mine/distribution/join");
        vo.setShareScene("recruit_" + distribution.getId());
        return vo;
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

    /**
     * 根据搜索条件构建分销员查询包装器。
     *
     * @param distributionSearchParams 搜索参数
     * @return 查询包装器，条件无匹配结果时为空
     */
    private Optional<LambdaQueryWrapper<Distribution>> buildMemberQueryWrapper(DistributionSearchParams distributionSearchParams) {
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
            List<String> parentDistributionIds = this.list(new LambdaQueryWrapper<Distribution>()
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
     * 批量加载会员对应的微信小程序 openid。
     *
     * @param memberIds 会员ID集合
     * @return 会员ID与 openid 的映射
     */
    private Map<String, String> loadOpenIdMap(Set<String> memberIds) {
        if (memberIds == null || memberIds.isEmpty()) {
            return Collections.emptyMap();
        }
        Map<String, String> openIdMap = new HashMap<>();
        connectService.list(new LambdaQueryWrapper<Connect>()
                        .in(Connect::getUserId, memberIds)
                        .eq(Connect::getUnionType, SourceEnum.WECHAT_MP_OPEN_ID.name()))
                .forEach(connect -> {
                    if (StringUtils.isNotEmpty(connect.getUserId()) && StringUtils.isNotEmpty(connect.getUnionId())) {
                        openIdMap.putIfAbsent(connect.getUserId(), connect.getUnionId());
                    }
                });
        return openIdMap;
    }

    /**
     * 根据手机号解析已通过审核的邀请人分销员ID。
     *
     * @param mobile 邀请人手机号
     * @return 分销员ID
     */
    private String resolveDistributionIdByMobile(String mobile) {
        Member inviterMember = memberService.getOne(new LambdaQueryWrapper<Member>()
                .eq(Member::getMobile, mobile)
                .last("limit 1"));
        if (inviterMember == null) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "邀请人手机号不正确，请核验后再提交！");
        }
        Distribution inviter = this.getOne(new LambdaQueryWrapper<Distribution>()
                .eq(Distribution::getMemberId, inviterMember.getId())
                .last("limit 1"));
        if (inviter == null || !DistributionStatusEnum.PASS.name().equals(inviter.getDistributionStatus())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "邀请人手机号不正确，请核验后再提交！");
        }
        return inviter.getId();
    }

    /**
     * 校验邀请关系是否形成环（邀请人不能是本人或其下级）。
     *
     * @param childDistributionId  子分销员ID
     * @param parentDistributionId 父分销员ID
     */
    private void validateInviteCycle(String childDistributionId, String parentDistributionId) {
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
            Distribution parent = getById(current);
            current = parent == null ? null : parent.getParentDistributionId();
        }
    }

    /**
     * 金额空值转零。
     *
     * @param amount 金额
     * @return 非空金额
     */
    private BigDecimal defaultAmount(BigDecimal amount) {
        return amount == null ? BigDecimal.ZERO : amount;
    }

    /**
     * 将分销员实体转换为列表展示 VO。
     *
     * @param distribution 分销员实体
     * @return 分销员 VO
     */
    private DistributionMemberVO toMemberVO(Distribution distribution) {
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
        Member member = memberService.getById(distribution.getMemberId());
        if (member != null) {
            vo.setMobile(member.getMobile());
            vo.setNickname(member.getNickName());
        }
        if (distribution.getGradeId() != null) {
            DistributionGrade grade = gradeService.resolveGrade(distribution.getGradeId());
            if (grade != null) {
                vo.setGradeName(grade.getGradeName());
            }
        }
        if (StringUtils.isNotEmpty(distribution.getParentDistributionId())) {
            vo.setInviterDistributionId(distribution.getParentDistributionId());
            Distribution inviter = getById(distribution.getParentDistributionId());
            if (inviter != null) {
                vo.setInviterNickname(inviter.getMemberName());
                Member inviterMember = memberService.getById(inviter.getMemberId());
                if (inviterMember != null) {
                    vo.setInviterMobile(inviterMember.getMobile());
                }
            }
        }
        return vo;
    }

    /**
     * 获取会员当前绑定的分销员ID。
     *
     * @param memberId 会员ID
     * @return 分销员ID，未绑定时为 null
     */
    private String getBoundDistributorId(String memberId) {
        DistributionCustomerRelation relation = customerRelationService.getActiveRelation(memberId);
        return relation == null ? null : relation.getDistributionId();
    }

    /**
     * 同步分销员邀请人数并刷新等级。
     *
     * @param distributionId 分销员ID
     */
    private void syncInviteCountAndGrade(String distributionId) {
        distributionParentBindSupport.syncInviteCountAndGrade(distributionId);
    }

}