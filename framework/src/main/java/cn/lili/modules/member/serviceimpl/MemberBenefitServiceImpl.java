package cn.lili.modules.member.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.SwitchEnum;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dos.MemberBenefit;
import cn.lili.modules.member.entity.dos.MemberBenefitDelivery;
import cn.lili.modules.member.entity.dos.MemberGrade;
import cn.lili.modules.member.entity.enums.MemberBenefitTypeEnum;
import cn.lili.modules.member.entity.vo.*;
import cn.lili.modules.member.mapper.MemberBenefitMapper;
import cn.lili.modules.member.mapper.MemberGradeBenefitGrantMapper;
import cn.lili.modules.member.mapper.MemberGradeMapper;
import cn.lili.modules.member.service.*;
import cn.lili.modules.member.support.MemberBenefitConfigSupport;
import cn.lili.modules.member.support.MemberBenefitCouponDisplaySupport;
import cn.lili.modules.member.support.MemberBenefitGrantSupport;
import cn.lili.modules.member.util.MemberBenefitRules;
import cn.lili.mybatis.util.PageUtil;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Stream;

/**
 * 客户权益服务实现，负责权益配置校验、当前等级及成长进度查询、自动权益发放和失败补发。
 * 协调配置校验、券礼包展示及实际发放支持类；逐项发放事务及防重交由权益发放服务处理。
 *
 * @author Bulbasaur
 * @since 2026-11-04
 */
@Slf4j
@Service
public class MemberBenefitServiceImpl extends ServiceImpl<MemberBenefitMapper, MemberBenefit> implements MemberBenefitService {

    @Autowired
    private MemberGradeMapper memberGradeMapper;
    @Autowired
    private MemberService memberService;
    @Autowired
    private MemberGradeService memberGradeService;
    @Autowired
    private MemberGradeBenefitGrantMapper memberGradeBenefitGrantMapper;
    @Autowired
    private MemberBenefitDeliveryService deliveryService;
    @Autowired
    private PaidMemberService paidMembers;
    @Autowired
    private MemberBenefitQueryService benefitQueries;
    @Autowired
    private MemberBenefitConfigSupport configSupport;
    @Autowired
    private MemberBenefitCouponDisplaySupport couponDisplaySupport;
    @Autowired
    private MemberBenefitGrantSupport grantSupport;

    /**
     * 新增权益，默认排序为1、状态为OPEN，完成类型及扩展配置校验后保存。
     *
     * @param memberBenefit 待新增权益，说明及扩展配置按权益类型校验
     * @throws ServiceException 参数非法或保存失败
     */
    @Override
    public void saveBenefit(MemberBenefit memberBenefit) {
        if (memberBenefit.getBenefitSort() == null) {
            memberBenefit.setBenefitSort(1);
        }
        if (CharSequenceUtil.isBlank(memberBenefit.getBenefitState())) {
            memberBenefit.setBenefitState(SwitchEnum.OPEN.name());
        }
        configSupport.validateBenefit(memberBenefit);
        if (!this.save(memberBenefit)) {
            throw new ServiceException(ResultCode.ERROR, "客户权益保存失败");
        }
    }

    /**
     * 更新已存在权益，未提供的排序、状态、类型及空白扩展配置沿用原值。
     * 已创建权益不允许变更类型；完整校验后按主键更新。
     *
     * @param id            权益主键
     * @param memberBenefit 待更新内容，主键以方法参数为准
     * @throws ServiceException 权益不存在、类型变更、参数非法或更新失败
     */
    @Override
    public void updateBenefit(String id, MemberBenefit memberBenefit) {
        MemberBenefit original = this.getById(id);
        if (original == null) {
            throw new ServiceException(ResultCode.ERROR, "客户权益不存在");
        }
        memberBenefit.setId(id);
        if (memberBenefit.getBenefitSort() == null) {
            memberBenefit.setBenefitSort(original.getBenefitSort());
        }
        if (CharSequenceUtil.isBlank(memberBenefit.getBenefitState())) {
            memberBenefit.setBenefitState(original.getBenefitState());
        }
        if (CharSequenceUtil.isBlank(memberBenefit.getBenefitType())) {
            memberBenefit.setBenefitType(original.getBenefitType());
        } else if (!memberBenefit.getBenefitType().equals(original.getBenefitType())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "已创建权益不允许修改类型");
        }
        if (CharSequenceUtil.isBlank(memberBenefit.getBenefitConfig())) {
            memberBenefit.setBenefitConfig(original.getBenefitConfig());
        }
        configSupport.validateBenefit(memberBenefit);
        if (!this.updateById(memberBenefit)) {
            throw new ServiceException(ResultCode.ERROR, "客户权益更新失败");
        }
    }

    /**
     * 校验权益存在性及OPEN/CLOSE状态值后切换启停状态。
     *
     * @param id    权益主键
     * @param state 目标状态，OPEN启用、CLOSE关闭
     * @throws ServiceException 权益不存在或状态非法
     */
    @Override
    public void updateBenefitState(String id, String state) {
        if (!SwitchEnum.OPEN.name().equals(state) && !SwitchEnum.CLOSE.name().equals(state)) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "权益状态非法");
        }
        MemberBenefit memberBenefit = this.getById(id);
        if (memberBenefit == null) {
            throw new ServiceException(ResultCode.ERROR, "客户权益不存在");
        }
        memberBenefit.setBenefitState(state);
        this.updateById(memberBenefit);
    }

    /**
     * 删除未被普通客户等级引用的权益，存在等级关联时拒绝删除。
     *
     * @param id 权益主键
     * @throws ServiceException 权益不存在、已被等级引用或删除失败
     */
    @Override
    public void deleteBenefit(String id) {
        MemberBenefit memberBenefit = this.getById(id);
        if (memberBenefit == null) {
            throw new ServiceException(ResultCode.ERROR, "客户权益不存在");
        }
        LambdaQueryWrapper<MemberGrade> gradeWrapper = new LambdaQueryWrapper<MemberGrade>()
                .isNotNull(MemberGrade::getBenefitIds)
                .apply("FIND_IN_SET({0}, benefit_ids)", id);
        Long count = memberGradeMapper.selectCount(gradeWrapper);
        if (count != null && count > 0) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "权益已被等级引用，无法删除");
        }
        if (!this.removeById(id)) {
            throw new ServiceException(ResultCode.ERROR, "客户权益删除失败");
        }
    }

    /**
     * 按名称及启停状态分页查询权益，排除已移除的会员折扣类型。
     *
     * @param keyword 权益名称模糊查询条件，可为空
     * @param state   OPEN/CLOSE筛选条件，可为空
     * @param page    分页参数
     * @return 按权益排序值升序的分页结果
     */
    @Override
    public IPage<MemberBenefit> getBenefitPage(String keyword, String state, PageVO page) {
        LambdaQueryWrapper<MemberBenefit> wrapper = this.getBenefitQueryWrapper(keyword, state, null);
        return this.page(PageUtil.initPage(page), wrapper);
    }

    /**
     * 查询权益列表，排除已移除的会员折扣类型并按排序值升序排列。
     *
     * @param onlyOpen true仅返回启用权益，其他值不按启停状态过滤
     * @return 权益列表
     */
    @Override
    public List<MemberBenefit> listBenefits(Boolean onlyOpen) {
        LambdaQueryWrapper<MemberBenefit> wrapper = this.getBenefitQueryWrapper(null, null, onlyOpen);
        return this.list(wrapper);
    }

    /**
     * 将当前支持的权益类型转换为管理端下拉选项。
     *
     * @return 包含类型编码和中文说明的列表
     */
    @Override
    public List<EnumValueVO> listBenefitTypes() {
        List<EnumValueVO> list = new ArrayList<>();
        for (MemberBenefitTypeEnum value : MemberBenefitTypeEnum.values()) {
            list.add(new EnumValueVO(value.value(), value.getDescription()));
        }
        return list;
    }

    /**
     * 按主键集合查询权益，排除会员折扣类型，按权益排序值排列而非入参顺序。
     *
     * @param ids      权益主键列表，空值或空列表直接返回空列表
     * @param onlyOpen true仅查询OPEN状态
     * @return 匹配的权益列表
     */
    @Override
    public List<MemberBenefit> listByIdList(List<String> ids, Boolean onlyOpen) {
        return benefitQueries.listByIdList(ids, onlyOpen);
    }

    /**
     * 读取客户绑定的有效普通等级，缺失或停用时按经验回退查询等级，仅返回开启权益。
     * 同时计算下一启用等级、所差经验及0～100的成长进度；查询不修改客户等级绑定。
     * 默认等级以0经验为进度起点，最高等级进度为100，无等级时为0。
     *
     * @param memberId 客户主键
     * @return 普通等级权益及升级经验进度，不合并付费会员权益列表
     * @throws ServiceException 客户不存在
     */
    @Override
    public MemberCurrentBenefitVO getCurrentMemberBenefits(String memberId) {
        Member member = memberService.getById(memberId);
        if (member == null) {
            throw new ServiceException(ResultCode.USER_NOT_EXIST);
        }

        MemberGrade grade = null;
        if (CharSequenceUtil.isNotBlank(member.getGradeId())) {
            grade = memberGradeService.getById(member.getGradeId());
        }
        if (grade == null || !SwitchEnum.OPEN.name().equals(grade.getGradeState())) {
            grade = memberGradeService.getCurrentGradeByExperience(member.getExperience());
        }
        List<String> benefitIds = grade == null || CharSequenceUtil.isBlank(grade.getBenefitIds())
                ? Collections.emptyList()
                : CharSequenceUtil.splitTrim(grade.getBenefitIds(), ',').stream()
                        .filter(CharSequenceUtil::isNotBlank).toList();
        List<MemberBenefit> benefits = this.listByIdList(benefitIds, true);
        benefits.forEach(b -> b.setBenefitDesc(MemberBenefitRules.description(b.getBenefitDesc())));
        MemberCurrentBenefitVO result = new MemberCurrentBenefitVO();
        result.setGradeId(grade == null ? null : grade.getId());
        result.setGrade(grade);
        result.setBenefits(benefits);
        long experience = member.getExperience() == null ? 0L : Math.max(0L, member.getExperience());
        result.setExperience(experience);
        long currentThreshold = grade == null || grade.getRequiredExperience() == null
                ? 0L : grade.getRequiredExperience().longValue();
        MemberGrade nextGrade = grade == null ? null : memberGradeService.listForUpgrade().stream()
                .filter(g -> SwitchEnum.OPEN.name().equals(g.getGradeState()))
                .filter(g -> g.getRequiredExperience() != null && g.getRequiredExperience() > currentThreshold)
                .min(Comparator.comparing(MemberGrade::getRequiredExperience)
                        .thenComparing(MemberGrade::getId)).orElse(null);
        result.setNextGrade(nextGrade);
        result.setHighestGrade(grade != null && nextGrade == null);
        result.setRemainingExperience(nextGrade == null ? 0L
                : Math.max(0L, nextGrade.getRequiredExperience().longValue() - experience));
        long baseline = grade != null && Boolean.TRUE.equals(grade.getIsDefault()) ? 0L : currentThreshold;
        int progress = nextGrade == null ? (grade == null ? 0 : 100)
                : (int) Math.max(0, Math.min(100, Math.floor(
                (experience - (double) baseline) * 100 / (nextGrade.getRequiredExperience() - baseline))));
        result.setUpgradeProgress(progress);
        return result;
    }

    /**
     * 查询管理端权益详情，券礼包额外组装券名称、面值和有效期等展示数据。
     *
     * @param id 权益主键
     * @return 权益及券礼包展示明细
     * @throws ServiceException 权益不存在
     */
    @Override
    public MemberBenefitDetailVO getManagerBenefitDetail(String id) {
        MemberBenefit benefit = this.getById(id);
        if (benefit == null) {
            throw new ServiceException(ResultCode.ERROR, "客户权益不存在");
        }
        MemberBenefitDetailVO vo = new MemberBenefitDetailVO();
        vo.setBenefit(benefit);
        if (MemberBenefitTypeEnum.COUPON_PACKAGE.name().equals(benefit.getBenefitType())) {
            vo.setCouponItems(couponDisplaySupport.buildCouponPackageDisplayItems(benefit.getBenefitConfig()));
        }
        return vo;
    }

    /**
     * 分页查询等级权益初始化记录，按客户手机号和等级筛选，不代表全部权益发放成功。
     *
     * @param mobile  客户手机号，精确匹配，可为空；号码不存在时返回空页
     * @param gradeId 等级主键，可为空
     * @param page    分页参数
     * @return 按初始化时间倒序排列的记录
     */
    @Override
    public IPage<MemberBenefitGrantRecordVO> pageBenefitGrantRecords(String mobile, String gradeId, PageVO page) {
        QueryWrapper<Object> wrapper = new QueryWrapper<>();
        wrapper.eq("g.delete_flag", 0);
        if (CharSequenceUtil.isNotBlank(gradeId)) {
            wrapper.eq("g.grade_id", gradeId.trim());
        }
        if (CharSequenceUtil.isNotBlank(mobile)) {
            Member m = this.memberService.findByMobile(mobile.trim());
            if (m == null) {
                wrapper.eq("g.member_id", "__NO_SUCH_MEMBER_FOR_GRANT_LOG__");
            } else {
                wrapper.eq("g.member_id", m.getId());
            }
        }
        wrapper.orderByDesc("g.create_time");
        Page<MemberBenefitGrantRecordVO> p = PageUtil.initPage(page);
        return this.memberGradeBenefitGrantMapper.pageGrantRecords(p, wrapper);
    }

    /**
     * 客户达到等级后尝试发放权益，有事务同步时注册提交后回调，否则直接执行。
     * 延后到经验更新提交后再开启发放事务，避免权益失败回滚升级或等待客户行锁。
     *
     * @param memberId 客户主键，空白值不处理
     * @param gradeId  达到的等级主键，空白值不处理
     */
    @Override
    public void tryGrantBenefitsForGradeReached(String memberId, String gradeId) {
        if (CharSequenceUtil.isBlank(memberId) || CharSequenceUtil.isBlank(gradeId)) return;
        // 经验值事务提交后再发放，避免发放失败回滚升级，以及新事务等待会员行锁。
        if (TransactionSynchronizationManager.isSynchronizationActive()) {
            TransactionSynchronizationManager.registerSynchronization(
                    new TransactionSynchronization() {
                        /** 升级事务提交后再发放，确保可读取已提交等级且不持有升级事务的客户行锁。 */
                        @Override
                        public void afterCommit() {
                            deliverGradeBenefits(memberId, gradeId);
                        }
                    });
        } else {
            deliverGradeBenefits(memberId, gradeId);
        }
    }

    /**
     * 重新读取客户当前普通等级，仅仍处于目标等级时委托逐项权益发放服务处理。
     * 捕获并记录失败，避免影响升级或调度中的其他客户，后续周期任务可再次扫描。
     *
     * @param memberId 客户主键
     * @param gradeId  本次拟发放的来源等级主键
     */
    private void deliverGradeBenefits(String memberId, String gradeId) {
        try {
            MemberCurrentBenefitVO current = getCurrentMemberBenefits(memberId);
            if (!gradeId.equals(current.getGradeId())) return;
            deliveryService.deliverGrade(memberId, gradeId, current.getBenefits(), grantSupport::grantBenefitToMember);
        } catch (Exception e) {
            log.error("等级权益发放失败 memberId={} gradeId={}", memberId, gradeId, e);
        }
    }

    /**
     * 按主键游标每批扫描200条，先处理有效付费订单，再处理客户当前普通等级。
     * 随后恢复PENDING及尝试次数小于10的FAILED明细，重试前重新检查来源资格。
     * 成功记录由账本防重；仅生成当前应发周期，不追补尚未生成的历史周期。
     */
    @Override
    public void processScheduledBenefits() {
        String paidCursor = "";
        while (true) {
            var paidOrders = paidMembers.activeOrders(paidCursor);
            if (paidOrders.isEmpty()) break;
            for (var order : paidOrders) {
                try {
                    deliverPaidMemberBenefits(order.getId());
                } catch (Exception e) {
                    log.warn("付费会员权益发放失败 orderId={}", order.getId(), e);
                }
            }
            paidCursor = paidOrders.get(paidOrders.size() - 1).getId();
        }
        String cursor = "";
        while (true) {
            List<Member> members = memberService.list(new LambdaQueryWrapper<Member>()
                    .gt(Member::getId, cursor).orderByAsc(Member::getId).last("limit 200"));
            if (members.isEmpty()) break;
            for (Member m : members) {
                try {
                    MemberCurrentBenefitVO current = getCurrentMemberBenefits(m.getId());
                    if (current.getGradeId() != null) deliverGradeBenefits(m.getId(), current.getGradeId());
                } catch (Exception e) {
                    log.warn("周期权益处理失败 memberId={}", m.getId(), e);
                }
            }
            cursor = members.get(members.size() - 1).getId();
        }
        // 恢复崩溃遗留的待发放任务，也重试上一周期已生成的失败记录。
        String deliveryCursor = "";
        while (true) {
            var pending = deliveryService.list(new LambdaQueryWrapper<MemberBenefitDelivery>()
                    .in(MemberBenefitDelivery::getStatus, "PENDING", "FAILED")
                    .lt(MemberBenefitDelivery::getAttempts, 10)
                    .gt(MemberBenefitDelivery::getId, deliveryCursor)
                    .orderByAsc(MemberBenefitDelivery::getId).last("limit 200"));
            if (pending.isEmpty()) break;
            for (var record : pending) {
                try {
                    var current = getCurrentMemberBenefits(record.getMemberId());
                    boolean eligible = "PAID_MEMBER".equals(record.getSourceType())
                            ? paidMembers.isBenefitEligible(record.getMemberId(), record.getSourceRef(), record.getBenefitId())
                            : record.getSourceRef().equals(current.getGradeId()) && current.getBenefits().stream().anyMatch(b -> b.getId().equals(record.getBenefitId()));
                    if (eligible) {
                        deliveryService.retry(record, grantSupport::grantBenefitToMember);
                    }
                } catch (Exception e) {
                    log.warn("权益自动补发失败 recordId={}", record.getId());
                }
            }
            deliveryCursor = pending.get(pending.size() - 1).getId();
        }
    }

    /**
     * 手动补发FAILED明细，按来源重新检查有效付费订单或当前等级及权益开启状态。
     * 实际发放沿用账本快照，手动入口不应用自动任务的10次尝试上限。
     *
     * @param id li_member_benefit_delivery发放明细主键
     * @throws ServiceException 明细不存在、非失败状态、资格失效或本次补发失败
     */
    @Override
    public void retryBenefitGrant(String id) {
        MemberBenefitDelivery record = deliveryService.getById(id);
        if (record == null || !"FAILED".equals(record.getStatus())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "仅失败记录可重试");
        }
        MemberCurrentBenefitVO current = getCurrentMemberBenefits(record.getMemberId());
        boolean eligible = "PAID_MEMBER".equals(record.getSourceType())
                ? paidMembers.isBenefitEligible(record.getMemberId(), record.getSourceRef(), record.getBenefitId())
                : "GRADE".equals(record.getSourceType()) && record.getSourceRef().equals(current.getGradeId())
                && current.getBenefits().stream().anyMatch(b -> b.getId().equals(record.getBenefitId()));
        if (!eligible) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "会员已不再享有该权益，不能补发");
        }
        deliveryService.retry(record, grantSupport::grantBenefitToMember);
    }

    /**
     * 合并普通等级与当前有效付费权益中的积分倍率，取最高值而不叠乘。
     *
     * @param memberId 客户主键
     * @return 最高倍率，无倍率权益时为1
     */
    @Override
    public BigDecimal getPointMultiplier(String memberId) {
        return Stream.concat(getCurrentMemberBenefits(memberId).getBenefits().stream(), paidMembers.activeBenefits(memberId).stream())
                .filter(b -> MemberBenefitTypeEnum.POINT_MULTIPLIER.name().equals(b.getBenefitType()))
                .map(b -> MemberBenefitRules.config(b.getBenefitConfig()).getBigDecimal("multiplier", BigDecimal.ONE))
                .max(BigDecimal::compareTo).orElse(BigDecimal.ONE);
    }

    /**
     * 按已支付且处于有效区间的付费订单快照发放仍开启的权益，未来续费区间暂不发放。
     * 来源订单和周期防重、事务及发放前再次核验由逐项发放服务执行。
     *
     * @param orderId li_paid_member_order主键，不是业务订单号
     */
    @Override
    public void deliverPaidMemberBenefits(String orderId) {
        var order = paidMembers.findById(orderId);
        if (order == null || !paidMembers.isActive(order)) return;
        deliveryService.deliverPaid(order, this.listByIdList(paidMembers.snapshot(order).getBenefitIds(), true), grantSupport::grantBenefitToMember);
    }

    /**
     * 构造权益公共查询条件，统一排除历史会员折扣类型并按排序值升序。
     *
     * @param keyword  权益名称模糊查询条件，可为空
     * @param state    显式状态筛选，可为空
     * @param onlyOpen true时额外限定OPEN
     * @return 服务端构建的权益查询条件
     */
    private LambdaQueryWrapper<MemberBenefit> getBenefitQueryWrapper(String keyword, String state, Boolean onlyOpen) {
        return new LambdaQueryWrapper<MemberBenefit>()
                .ne(MemberBenefit::getBenefitType, "MEMBER_DISCOUNT")
                .eq(Boolean.TRUE.equals(onlyOpen), MemberBenefit::getBenefitState, SwitchEnum.OPEN.name())
                .eq(CharSequenceUtil.isNotBlank(state), MemberBenefit::getBenefitState, state)
                .like(CharSequenceUtil.isNotBlank(keyword), MemberBenefit::getBenefitName, keyword)
                .orderByAsc(MemberBenefit::getBenefitSort);
    }
}
