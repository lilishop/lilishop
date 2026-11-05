package cn.lili.modules.member.serviceimpl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONArray;
import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.SwitchEnum;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dos.MemberBenefit;
import cn.lili.modules.member.entity.dos.MemberGrade;
import cn.lili.modules.member.entity.enums.MemberBenefitTypeEnum;
import cn.lili.modules.member.entity.enums.PointTypeEnum;
import cn.lili.modules.member.entity.vo.*;
import cn.lili.modules.member.mapper.MemberBenefitMapper;
import cn.lili.modules.member.mapper.MemberGradeBenefitGrantMapper;
import cn.lili.modules.member.mapper.MemberGradeMapper;
import cn.lili.modules.member.service.*;
import cn.lili.modules.promotion.entity.dos.Coupon;
import cn.lili.modules.promotion.entity.enums.CouponRangeDayEnum;
import cn.lili.modules.promotion.entity.enums.CouponTypeEnum;
import cn.lili.modules.promotion.service.CouponService;
import cn.lili.modules.promotion.service.MemberCouponService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.*;

/**
 * 客户权益服务实现，负责权益配置校验、当前等级及成长进度查询、自动权益发放和失败补发。
 * 计算当前普通等级权益的最高积分倍率，逐项发放事务及防重交由权益发放服务处理。
 *
 * @author Bulbasaur
 * @since 2026-11-04
 */
@Slf4j
@Service
public class MemberBenefitServiceImpl extends ServiceImpl<MemberBenefitMapper, MemberBenefit> implements MemberBenefitService {

    /**
     * 赠送积分权益：benefitConfig(JSON) 中 giftPoint 字段最大值
     */
    private static final int GIFT_POINT_CONFIG_MAX = 99999;

    /**
     * 券礼包每张优惠券单次赠送份数下限，单位张。
     */
    private static final int COUPON_PKG_QTY_MIN = 1;
    /**
     * 券礼包每张优惠券单次赠送份数上限，单位张。
     */
    private static final int COUPON_PKG_QTY_MAX = 10;

    @Autowired
    private MemberGradeMapper memberGradeMapper;
    @Autowired
    private MemberService memberService;
    @Autowired
    private MemberGradeService memberGradeService;
    @Autowired
    private CouponService couponService;
    @Autowired
    private MemberCouponService memberCouponService;
    @Autowired
    private MemberGradeBenefitGrantMapper memberGradeBenefitGrantMapper;
    @Autowired
    private MemberBenefitDeliveryService deliveryService;
    @Autowired
    private MemberBenefitQueryService benefitQueries;

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
        this.validateBenefit(memberBenefit);
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
        this.validateBenefit(memberBenefit);
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
        List<String> benefitIds = new ArrayList<>();
        if (grade != null && CharSequenceUtil.isNotBlank(grade.getBenefitIds())) {
            List<String> parts = CharSequenceUtil.splitTrim(grade.getBenefitIds(), ',');
            if (CollUtil.isNotEmpty(parts)) {
                for (String part : parts) {
                    if (CharSequenceUtil.isNotBlank(part)) {
                        benefitIds.add(part);
                    }
                }
            }
        }
        List<MemberBenefit> benefits = this.listByIdList(benefitIds, true);
        benefits.forEach(b -> b.setBenefitDesc(cn.lili.modules.member.util.MemberBenefitRules.description(b.getBenefitDesc())));
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
                .min(java.util.Comparator.comparing(MemberGrade::getRequiredExperience)
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
            vo.setCouponItems(this.buildCouponPackageDisplayItems(benefit.getBenefitConfig()));
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
        if (org.springframework.transaction.support.TransactionSynchronizationManager.isSynchronizationActive()) {
            org.springframework.transaction.support.TransactionSynchronizationManager.registerSynchronization(
                    new org.springframework.transaction.support.TransactionSynchronization() {
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
            deliveryService.deliverGrade(memberId, gradeId, current.getBenefits(), this::grantBenefitToMember);
        } catch (Exception e) {
            log.error("等级权益发放失败 memberId={} gradeId={}", memberId, gradeId, e);
        }
    }

    /**
     * 按主键游标每批扫描200条，处理客户当前普通等级权益。
     * 随后恢复PENDING及尝试次数小于10的FAILED明细，重试前重新检查来源资格。
     * 成功记录由账本防重；仅生成当前应发周期，不追补尚未生成的历史周期。
     */
    @Override
    public void processScheduledBenefits() {
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
            var pending = deliveryService.list(new LambdaQueryWrapper<cn.lili.modules.member.entity.dos.MemberBenefitDelivery>()
                    .in(cn.lili.modules.member.entity.dos.MemberBenefitDelivery::getStatus, "PENDING", "FAILED")
                    .lt(cn.lili.modules.member.entity.dos.MemberBenefitDelivery::getAttempts, 10)
                    .gt(cn.lili.modules.member.entity.dos.MemberBenefitDelivery::getId, deliveryCursor)
                    .orderByAsc(cn.lili.modules.member.entity.dos.MemberBenefitDelivery::getId).last("limit 200"));
            if (pending.isEmpty()) break;
            for (var record : pending) {
                try {
                    var current = getCurrentMemberBenefits(record.getMemberId());
                    boolean eligible = "GRADE".equals(record.getSourceType()) && record.getSourceRef().equals(current.getGradeId()) && current.getBenefits().stream().anyMatch(b -> b.getId().equals(record.getBenefitId()));
                    if (eligible) {
                        deliveryService.retry(record, this::grantBenefitToMember);
                    }
                } catch (Exception e) {
                    log.warn("权益自动补发失败 recordId={}", record.getId());
                }
            }
            deliveryCursor = pending.get(pending.size() - 1).getId();
        }
    }

    /**
     * 手动补发FAILED明细，重新检查当前等级及权益开启状态。
     * 实际发放沿用账本快照，手动入口不应用自动任务的10次尝试上限。
     *
     * @param id li_member_benefit_delivery发放明细主键
     * @throws ServiceException 明细不存在、非失败状态、资格失效或本次补发失败
     */
    @Override
    public void retryBenefitGrant(String id) {
        cn.lili.modules.member.entity.dos.MemberBenefitDelivery record = deliveryService.getById(id);
        if (record == null || !"FAILED".equals(record.getStatus())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "仅失败记录可重试");
        }
        MemberCurrentBenefitVO current = getCurrentMemberBenefits(record.getMemberId());
        boolean eligible = "GRADE".equals(record.getSourceType()) && record.getSourceRef().equals(current.getGradeId())
                && current.getBenefits().stream().anyMatch(b -> b.getId().equals(record.getBenefitId()));
        if (!eligible) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "会员已不再享有该权益，不能补发");
        }
        deliveryService.retry(record, this::grantBenefitToMember);
    }

    /**
     * 获取普通等级当前有效权益中的最高积分倍率，取最高值而不叠乘。
     *
     * @param memberId 客户主键
     * @return 最高倍率，无倍率权益时为1
     */
    @Override
    public java.math.BigDecimal getPointMultiplier(String memberId) {
        return getCurrentMemberBenefits(memberId).getBenefits().stream()
                .filter(b -> MemberBenefitTypeEnum.POINT_MULTIPLIER.name().equals(b.getBenefitType()))
                .map(b -> cn.lili.modules.member.util.MemberBenefitRules.config(b.getBenefitConfig()).getBigDecimal("multiplier", java.math.BigDecimal.ONE))
                .max(java.math.BigDecimal::compareTo).orElse(java.math.BigDecimal.ONE);
    }


    /**
     * 执行单项自动权益回调：赠送积分或逐份领取券礼包，其他类型不执行自动履约。
     * 调用方提供已校验的权益快照并管理事务；积分更新或领券抛异常时由发放服务回滚并记录失败。
     *
     * @param member  已锁定的领取客户
     * @param benefit 发放账本还原的权益名称、类型及配置快照
     */
    private void grantBenefitToMember(Member member, MemberBenefit benefit) {
        String memberName = CharSequenceUtil.blankToDefault(member.getUsername(), member.getNickName());
        if (MemberBenefitTypeEnum.GIFT_POINT.name().equals(benefit.getBenefitType())) {
            Integer gp = this.parseGiftPointFromConfig(benefit.getBenefitConfig());
            if (gp != null && gp > 0) {
                if (!Boolean.TRUE.equals(this.memberService.updateMemberPoint(gp.longValue(), PointTypeEnum.INCREASE.name(), member.getId(),
                        "等级权益「" + benefit.getBenefitName() + "」赠送积分",
                        cn.lili.modules.member.entity.enums.PointSourceEnum.BENEFIT.name())))
                    throw new IllegalStateException("积分更新失败");
            }
            return;
        }
        if (MemberBenefitTypeEnum.COUPON_PACKAGE.name().equals(benefit.getBenefitType())) {
            if (CharSequenceUtil.isBlank(benefit.getBenefitConfig())) {
                return;
            }
            JSONObject obj;
            try {
                obj = JSONUtil.parseObj(benefit.getBenefitConfig());
            } catch (Exception e) {
                log.warn("券礼包权益配置解析失败 benefitId={}", benefit.getId(), e);
                return;
            }
            JSONArray coupons = obj.getJSONArray("coupons");
            if (coupons == null || coupons.isEmpty()) {
                return;
            }
            for (int i = 0; i < coupons.size(); i++) {
                JSONObject item = coupons.getJSONObject(i);
                if (item == null) {
                    continue;
                }
                String couponId = item.getStr("couponId");
                Object qtyRaw = item.get("quantity");
                if (CharSequenceUtil.isBlank(couponId) || qtyRaw == null) {
                    continue;
                }
                int quantity;
                try {
                    if (qtyRaw instanceof Number) {
                        quantity = ((Number) qtyRaw).intValue();
                    } else {
                        quantity = Integer.parseInt(qtyRaw.toString().trim());
                    }
                } catch (NumberFormatException e) {
                    continue;
                }
                if (quantity < 1) {
                    continue;
                }
                for (int k = 0; k < quantity; k++) {
                    this.memberCouponService.receiveCoupon(couponId.trim(), member.getId(), memberName);
                }
            }
        }
    }

    /**
     * 从权益扩展JSON解析giftPoint，发放前的合法范围由配置保存校验保证。
     *
     * @param benefitConfig 权益扩展配置JSON
     * @return 配置的赠送积分数，缺失、空白或解析异常时为null
     */
    private Integer parseGiftPointFromConfig(String benefitConfig) {
        if (CharSequenceUtil.isBlank(benefitConfig)) {
            return null;
        }
        try {
            JSONObject obj = JSONUtil.parseObj(benefitConfig);
            Object raw = obj.get("giftPoint");
            if (raw == null) {
                return null;
            }
            if (raw instanceof Number) {
                return ((Number) raw).intValue();
            }
            return Integer.parseInt(raw.toString().trim());
        } catch (Exception e) {
            return null;
        }
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

    /**
     * 校验权益必填信息、类型、排序、状态及类型专属配置，并清理说明富文本。
     * 积分倍率范围1～100且最多两位小数，赠送积分为0～99999整数，券礼包另校验周期及平台券。
     *
     * @param memberBenefit 待保存权益，说明会原地替换为清理后的内容
     * @throws ServiceException 必填信息、范围、精度或类型配置非法
     */
    private void validateBenefit(MemberBenefit memberBenefit) {
        if (memberBenefit == null) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "客户权益参数不能为空");
        }
        if (CharSequenceUtil.isBlank(memberBenefit.getBenefitName())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "权益名称不能为空");
        }
        if (!MemberBenefitTypeEnum.isValid(memberBenefit.getBenefitType())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "权益类型非法");
        }
        if (memberBenefit.getBenefitSort() == null || memberBenefit.getBenefitSort() < 1) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "权益排序必须为正整数");
        }
        if (!SwitchEnum.OPEN.name().equals(memberBenefit.getBenefitState()) && !SwitchEnum.CLOSE.name().equals(memberBenefit.getBenefitState())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "权益状态非法");
        }

        if (CharSequenceUtil.isBlank(memberBenefit.getBenefitLogo()))
            throw new ServiceException(ResultCode.PARAMS_ERROR, "权益图标不能为空");
        if (CharSequenceUtil.isBlank(memberBenefit.getBenefitDesc()))
            throw new ServiceException(ResultCode.PARAMS_ERROR, "权益介绍不能为空");
        memberBenefit.setBenefitDesc(cn.lili.modules.member.util.MemberBenefitRules.description(memberBenefit.getBenefitDesc()));
        cn.hutool.json.JSONObject config = cn.lili.modules.member.util.MemberBenefitRules.config(memberBenefit.getBenefitConfig());
        if (MemberBenefitTypeEnum.POINT_MULTIPLIER.name().equals(memberBenefit.getBenefitType())) {
            cn.lili.modules.member.util.MemberBenefitRules.number(config, "multiplier", java.math.BigDecimal.ONE, java.math.BigDecimal.valueOf(100), 2);
        }
        if (MemberBenefitTypeEnum.COUPON_PACKAGE.name().equals(memberBenefit.getBenefitType())) {
            cn.lili.modules.member.util.MemberBenefitRules.validateSchedule(config);
        }
        if (MemberBenefitTypeEnum.GIFT_POINT.name().equals(memberBenefit.getBenefitType())) {
            cn.lili.modules.member.util.MemberBenefitRules.number(config, "giftPoint", java.math.BigDecimal.ZERO, java.math.BigDecimal.valueOf(99999), 0);
            this.validateGiftPointConfig(memberBenefit.getBenefitConfig());
        }
        if (MemberBenefitTypeEnum.COUPON_PACKAGE.name().equals(memberBenefit.getBenefitType())) {
            this.validateCouponPackageConfig(memberBenefit.getBenefitConfig());
        }
    }

    /**
     * 校验赠送积分扩展配置包含giftPoint，范围0～99999。
     * 调用前由通用数值校验保证整数精度，本方法继续校验配置存在性及积分范围。
     *
     * @param benefitConfig 权益扩展配置JSON
     * @throws ServiceException 配置为空、JSON非法、缺少giftPoint或积分不合法
     */
    private void validateGiftPointConfig(String benefitConfig) {
        if (CharSequenceUtil.isBlank(benefitConfig)) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "赠送积分权益需填写赠送积分数量（扩展配置 JSON 中设置 giftPoint）");
        }
        JSONObject obj;
        try {
            obj = JSONUtil.parseObj(benefitConfig);
        } catch (Exception e) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "赠送积分权益的扩展配置须为合法 JSON");
        }
        if (obj == null || obj.isEmpty()) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "赠送积分权益需填写赠送积分数量（扩展配置 JSON 中设置 giftPoint）");
        }
        Object raw = obj.get("giftPoint");
        if (raw == null) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "赠送积分权益需在扩展配置中设置 giftPoint（0-" + GIFT_POINT_CONFIG_MAX + "）");
        }
        int giftPoint;
        try {
            if (raw instanceof Number) {
                giftPoint = ((Number) raw).intValue();
            } else {
                giftPoint = Integer.parseInt(raw.toString().trim());
            }
        } catch (NumberFormatException e) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "赠送积分 giftPoint 须为整数");
        }
        if (giftPoint < 0 || giftPoint > GIFT_POINT_CONFIG_MAX) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "赠送积分范围为 0-" + GIFT_POINT_CONFIG_MAX);
        }
    }

    /**
     * 校验券礼包最多配置10种不重复的平台券，每种数量为1～10的整数。
     * 逐项检查couponId、quantity及平台归属，发放时的实际可领取性由领券服务判断。
     *
     * @param benefitConfig 含coupons数组的权益扩展配置JSON
     * @throws ServiceException JSON、列表、平台券关联或赠送数量非法
     */
    private void validateCouponPackageConfig(String benefitConfig) {
        if (CharSequenceUtil.isBlank(benefitConfig)) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "券礼包权益需配置优惠券（扩展配置 JSON，含 coupons 列表）");
        }
        JSONObject obj;
        try {
            obj = JSONUtil.parseObj(benefitConfig);
        } catch (Exception e) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "券礼包权益的扩展配置须为合法 JSON");
        }
        if (obj == null || obj.isEmpty()) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "券礼包权益需配置 coupons");
        }
        JSONArray coupons = obj.getJSONArray("coupons");
        if (coupons == null || coupons.isEmpty()) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "券礼包请至少配置一张优惠券（coupons 数组内需包含 couponId、quantity）");
        }
        if (coupons.size() > 10) throw new ServiceException(ResultCode.PARAMS_ERROR, "券礼包最多选择10张优惠券");
        Set<String> couponIds = new HashSet<>();
        for (int i = 0; i < coupons.size(); i++) {
            JSONObject item = coupons.getJSONObject(i);
            if (item == null || item.isEmpty()) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "券礼包 coupons[" + i + "] 格式不正确");
            }
            String couponId = item.getStr("couponId");
            if (CharSequenceUtil.isBlank(couponId)) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "券礼包 coupons[" + i + "] 缺少 couponId（平台优惠券 ID）");
            }
            if (!couponIds.add(couponId.trim())) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "券礼包中同一优惠券不能重复：" + couponId);
            }
            cn.lili.modules.member.util.MemberBenefitRules.number(item, "quantity", java.math.BigDecimal.ONE, java.math.BigDecimal.TEN, 0);
            Coupon configuredCoupon = couponService.getById(couponId.trim());
            if (configuredCoupon == null || !"0".equals(configuredCoupon.getStoreId()))
                throw new ServiceException(ResultCode.PARAMS_ERROR, "请选择有效的平台优惠券");
            Object qtyRaw = item.get("quantity");
            if (qtyRaw == null) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "券礼包 coupons[" + i + "] 缺少 quantity（赠送份数 " + COUPON_PKG_QTY_MIN + "-" + COUPON_PKG_QTY_MAX + "）");
            }
            int quantity;
            try {
                if (qtyRaw instanceof Number) {
                    quantity = ((Number) qtyRaw).intValue();
                } else {
                    quantity = Integer.parseInt(qtyRaw.toString().trim());
                }
            } catch (NumberFormatException e) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "券礼包 quantity 须为整数");
            }
            if (quantity < COUPON_PKG_QTY_MIN || quantity > COUPON_PKG_QTY_MAX) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "券礼包每张优惠券赠送份数须为 " + COUPON_PKG_QTY_MIN + "-" + COUPON_PKG_QTY_MAX);
            }
        }
    }

    /**
     * 将券礼包配置转换为展示行，关联当前优惠券的名称、面值及有效期。
     * 配置空白或解析失败时返回空列表；优惠券缺失时保留配置中的主键和数量。
     *
     * @param benefitConfig 券礼包扩展配置JSON
     * @return 券礼包展示明细，顺序与配置数组一致
     */
    private List<MemberBenefitCouponItemVO> buildCouponPackageDisplayItems(String benefitConfig) {
        List<MemberBenefitCouponItemVO> list = new ArrayList<>();
        if (CharSequenceUtil.isBlank(benefitConfig)) {
            return list;
        }
        JSONObject obj;
        try {
            obj = JSONUtil.parseObj(benefitConfig);
        } catch (Exception e) {
            return list;
        }
        JSONArray coupons = obj.getJSONArray("coupons");
        if (coupons == null || coupons.isEmpty()) {
            return list;
        }
        for (int i = 0; i < coupons.size(); i++) {
            JSONObject item = coupons.getJSONObject(i);
            if (item == null) {
                continue;
            }
            String couponId = item.getStr("couponId");
            Integer quantity = this.parseCouponQuantity(item.get("quantity"));
            MemberBenefitCouponItemVO row = new MemberBenefitCouponItemVO();
            row.setCouponId(CharSequenceUtil.isNotBlank(couponId) ? couponId.trim() : null);
            row.setQuantity(quantity);
            if (CharSequenceUtil.isNotBlank(couponId)) {
                Coupon coupon = this.couponService.getById(couponId.trim());
                if (coupon != null) {
                    row.setCouponName(coupon.getCouponName());
                    row.setCouponType(coupon.getCouponType());
                    row.setPrice(coupon.getPrice());
                    row.setCouponDiscount(coupon.getCouponDiscount());
                    row.setFaceValueText(this.buildCouponFaceValueText(coupon));
                    row.setValidityText(this.buildCouponValidityText(coupon));
                }
            }
            list.add(row);
        }
        return list;
    }

    /**
     * 解析券礼包展示数量，支持数字及整数字符串，不在此校验业务范围。
     *
     * @param qtyRaw 配置中的原始quantity值
     * @return 解析后的数量，缺失或字符串解析失败时为null
     */
    private Integer parseCouponQuantity(Object qtyRaw) {
        if (qtyRaw == null) {
            return null;
        }
        try {
            if (qtyRaw instanceof Number) {
                return ((Number) qtyRaw).intValue();
            }
            return Integer.parseInt(qtyRaw.toString().trim());
        } catch (NumberFormatException e) {
            return null;
        }
    }

    /**
     * 格式化优惠券面值：现金券显示减免金额（元），折扣券显示折数。
     *
     * @param coupon 优惠券，可为null
     * @return 展示文本；缺失券或类型时为null，无法识别的面值为短横线
     */
    private String buildCouponFaceValueText(Coupon coupon) {
        if (coupon == null || coupon.getCouponType() == null) {
            return null;
        }
        if (CouponTypeEnum.PRICE.name().equals(coupon.getCouponType()) && coupon.getPrice() != null) {
            return "减免¥" + coupon.getPrice();
        }
        if (CouponTypeEnum.DISCOUNT.name().equals(coupon.getCouponType()) && coupon.getCouponDiscount() != null) {
            return coupon.getCouponDiscount() + "折";
        }
        return "-";
    }

    /**
     * 格式化优惠券有效期，固定区间显示起止时间，动态有效期显示领取后有效天数。
     *
     * @param coupon 优惠券，可为null
     * @return 有效期说明，信息不足时为null
     */
    private String buildCouponValidityText(Coupon coupon) {
        if (coupon == null || coupon.getRangeDayType() == null) {
            Date st = coupon != null ? coupon.getStartTime() : null;
            Date et = coupon != null ? coupon.getEndTime() : null;
            if (st != null || et != null) {
                String a = st != null ? DateUtil.formatDateTime(st) : "";
                String b = et != null ? DateUtil.formatDateTime(et) : "";
                if (CharSequenceUtil.isNotBlank(a) && CharSequenceUtil.isNotBlank(b)) {
                    return a + " ~ " + b;
                }
            }
            return null;
        }
        if (CouponRangeDayEnum.FIXEDTIME.name().equals(coupon.getRangeDayType())) {
            Date st = coupon.getStartTime();
            Date et = coupon.getEndTime();
            if (st != null && et != null) {
                return DateUtil.formatDateTime(st) + " ~ " + DateUtil.formatDateTime(et);
            }
            if (st != null) {
                return DateUtil.formatDateTime(st) + " 起";
            }
            if (et != null) {
                return "至 " + DateUtil.formatDateTime(et);
            }
        }
        if (CouponRangeDayEnum.DYNAMICTIME.name().equals(coupon.getRangeDayType())
                && coupon.getEffectiveDays() != null
                && coupon.getEffectiveDays() > 0) {
            return "领取后" + coupon.getEffectiveDays() + "天内有效";
        }
        return null;
    }
}
