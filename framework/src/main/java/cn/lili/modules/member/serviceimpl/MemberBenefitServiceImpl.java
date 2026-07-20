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
import cn.lili.modules.member.entity.enums.MemberBenefitTypeEnum;
import cn.lili.modules.member.entity.enums.PointTypeEnum;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dos.MemberBenefit;
import cn.lili.modules.member.entity.dos.MemberGrade;
import cn.lili.modules.member.entity.dos.MemberGradeBenefitGrant;
import cn.lili.modules.member.entity.vo.EnumValueVO;
import cn.lili.modules.member.entity.vo.MemberBenefitCouponItemVO;
import cn.lili.modules.member.entity.vo.MemberBenefitDetailVO;
import cn.lili.modules.member.entity.vo.MemberBenefitGrantRecordVO;
import cn.lili.modules.member.entity.vo.MemberCurrentBenefitVO;
import cn.lili.modules.member.mapper.MemberBenefitMapper;
import cn.lili.modules.member.mapper.MemberGradeBenefitGrantMapper;
import cn.lili.modules.member.mapper.MemberGradeMapper;
import cn.lili.modules.member.service.MemberBenefitService;
import cn.lili.modules.member.service.MemberGradeService;
import cn.lili.modules.member.service.MemberService;
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
import org.springframework.dao.DuplicateKeyException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Slf4j
@Service
public class MemberBenefitServiceImpl extends ServiceImpl<MemberBenefitMapper, MemberBenefit> implements MemberBenefitService {

    /** 赠送积分权益：benefitConfig(JSON) 中 giftPoint 字段最大值 */
    private static final int GIFT_POINT_CONFIG_MAX = 99999;

    /** 券礼包：每张优惠券单次赠送份数（与需求文档一致） */
    private static final int COUPON_PKG_QTY_MIN = 1;
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
        }
        if (CharSequenceUtil.isBlank(memberBenefit.getBenefitConfig())) {
            memberBenefit.setBenefitConfig(original.getBenefitConfig());
        }
        this.validateBenefit(memberBenefit);
        if (!this.updateById(memberBenefit)) {
            throw new ServiceException(ResultCode.ERROR, "客户权益更新失败");
        }
    }

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

    @Override
    public IPage<MemberBenefit> getBenefitPage(String keyword, String state, PageVO page) {
        LambdaQueryWrapper<MemberBenefit> wrapper = this.getBenefitQueryWrapper(keyword, state, null);
        return this.page(PageUtil.initPage(page), wrapper);
    }

    @Override
    public List<MemberBenefit> listBenefits(Boolean onlyOpen) {
        LambdaQueryWrapper<MemberBenefit> wrapper = this.getBenefitQueryWrapper(null, null, onlyOpen);
        return this.list(wrapper);
    }

    @Override
    public List<EnumValueVO> listBenefitTypes() {
        List<EnumValueVO> list = new ArrayList<>();
        for (MemberBenefitTypeEnum value : MemberBenefitTypeEnum.values()) {
            list.add(new EnumValueVO(value.value(), value.getDescription()));
        }
        return list;
    }

    @Override
    public List<MemberBenefit> listByIdList(List<String> ids, Boolean onlyOpen) {
        if (CollUtil.isEmpty(ids)) {
            return Collections.emptyList();
        }
        LambdaQueryWrapper<MemberBenefit> wrapper = new LambdaQueryWrapper<MemberBenefit>()
                .in(MemberBenefit::getId, ids)
                .eq(Boolean.TRUE.equals(onlyOpen), MemberBenefit::getBenefitState, SwitchEnum.OPEN.name())
                .orderByAsc(MemberBenefit::getBenefitSort);
        return this.list(wrapper);
    }

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
        if (grade == null) {
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
        return new MemberCurrentBenefitVO(grade == null ? null : grade.getId(), grade, benefits);
    }

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

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void tryGrantBenefitsForGradeReached(String memberId, String gradeId) {
        if (CharSequenceUtil.isBlank(memberId) || CharSequenceUtil.isBlank(gradeId)) {
            return;
        }
        Member member = memberService.getById(memberId);
        if (member == null) {
            throw new ServiceException(ResultCode.USER_NOT_EXIST);
        }
        MemberGrade grade = memberGradeService.getById(gradeId);
        if (grade == null) {
            return;
        }
        MemberGradeBenefitGrant claim = new MemberGradeBenefitGrant();
        claim.setMemberId(memberId);
        claim.setGradeId(gradeId);
        try {
            this.memberGradeBenefitGrantMapper.insert(claim);
        } catch (DuplicateKeyException e) {
            return;
        }
        String memberName = CharSequenceUtil.blankToDefault(member.getUsername(), member.getNickName());
        if (CharSequenceUtil.isBlank(grade.getBenefitIds())) {
            return;
        }
        List<String> benefitIds = CharSequenceUtil.splitTrim(grade.getBenefitIds(), ',');
        for (String bid : benefitIds) {
            if (CharSequenceUtil.isBlank(bid)) {
                continue;
            }
            MemberBenefit benefit = this.getById(bid.trim());
            if (benefit == null || !SwitchEnum.OPEN.name().equals(benefit.getBenefitState())) {
                continue;
            }
            this.grantBenefitToMember(member, memberName, benefit);
        }
    }

    /**
     * 发放单条权益（赠送积分、券礼包）；其它类型暂无自动发放。
     */
    private void grantBenefitToMember(Member member, String memberName, MemberBenefit benefit) {
        if (MemberBenefitTypeEnum.GIFT_POINT.name().equals(benefit.getBenefitType())) {
            Integer gp = this.parseGiftPointFromConfig(benefit.getBenefitConfig());
            if (gp != null && gp > 0) {
                this.memberService.updateMemberPoint(gp.longValue(), PointTypeEnum.INCREASE.name(), member.getId(),
                        "等级权益「" + benefit.getBenefitName() + "」赠送积分",
                        cn.lili.modules.member.entity.enums.PointSourceEnum.BENEFIT.name());
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

    private LambdaQueryWrapper<MemberBenefit> getBenefitQueryWrapper(String keyword, String state, Boolean onlyOpen) {
        return new LambdaQueryWrapper<MemberBenefit>()
                .eq(Boolean.TRUE.equals(onlyOpen), MemberBenefit::getBenefitState, SwitchEnum.OPEN.name())
                .eq(CharSequenceUtil.isNotBlank(state), MemberBenefit::getBenefitState, state)
                .like(CharSequenceUtil.isNotBlank(keyword), MemberBenefit::getBenefitName, keyword)
                .orderByAsc(MemberBenefit::getBenefitSort);
    }

    private void validateBenefit(MemberBenefit memberBenefit) {
        if (memberBenefit == null) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "客户权益参数不能为空");
        }
        if (CharSequenceUtil.isBlank(memberBenefit.getBenefitName())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "权益名称不能为空");
        }
        if (CharSequenceUtil.isNotBlank(memberBenefit.getBenefitType()) && !MemberBenefitTypeEnum.isValid(memberBenefit.getBenefitType())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "权益类型非法");
        }
        if (memberBenefit.getBenefitSort() == null || memberBenefit.getBenefitSort() < 1) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "权益排序必须为正整数");
        }
        if (!SwitchEnum.OPEN.name().equals(memberBenefit.getBenefitState()) && !SwitchEnum.CLOSE.name().equals(memberBenefit.getBenefitState())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "权益状态非法");
        }

        if (MemberBenefitTypeEnum.GIFT_POINT.name().equals(memberBenefit.getBenefitType())) {
            this.validateGiftPointConfig(memberBenefit.getBenefitConfig());
        }
        if (MemberBenefitTypeEnum.COUPON_PACKAGE.name().equals(memberBenefit.getBenefitType())) {
            this.validateCouponPackageConfig(memberBenefit.getBenefitConfig());
        }
    }

    /**
     * 赠送积分类型：扩展配置须为 JSON，且包含 giftPoint（0～99999）。
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
     * 券礼包：扩展配置为 JSON，须包含 coupons 数组；每项为平台优惠券 couponId + 赠送份数 quantity（1～10）。
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
