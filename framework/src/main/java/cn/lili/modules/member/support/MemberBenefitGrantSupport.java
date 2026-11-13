package cn.lili.modules.member.support;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONArray;
import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dos.MemberBenefit;
import cn.lili.modules.member.entity.enums.MemberBenefitTypeEnum;
import cn.lili.modules.member.entity.enums.PointSourceEnum;
import cn.lili.modules.member.entity.enums.PointTypeEnum;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.promotion.service.MemberCouponService;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * 权益发放回调，按账本快照赠送积分及优惠券；事务、资格核验和幂等由调用方发放服务管理。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
@Slf4j
@Component
public class MemberBenefitGrantSupport {

    private final MemberService memberService;
    private final MemberCouponService memberCouponService;

    /**
     * 注入积分和领券服务，不另启事务，发放异常继续向上传递以触发单项回滚。
     *
     * @param memberService 客户积分服务
     * @param memberCouponService 客户领券服务
     */
    public MemberBenefitGrantSupport(MemberService memberService, MemberCouponService memberCouponService) {
        this.memberService = memberService;
        this.memberCouponService = memberCouponService;
    }

    /**
     * 执行单项自动权益回调：赠送积分或逐份领取券礼包，其他类型不执行自动履约。
     * 调用方提供已校验的权益快照并管理事务；积分更新或领券抛异常时由发放服务回滚并记录失败。
     *
     * @param member  已锁定的领取客户
     * @param benefit 发放账本还原的权益名称、类型及配置快照
     */
    public void grantBenefitToMember(Member member, MemberBenefit benefit) {
        String memberName = CharSequenceUtil.blankToDefault(member.getUsername(), member.getNickName());
        if (MemberBenefitTypeEnum.GIFT_POINT.name().equals(benefit.getBenefitType())) {
            Integer gp = this.parseGiftPointFromConfig(benefit.getBenefitConfig());
            if (gp != null && gp > 0) {
                if (!Boolean.TRUE.equals(this.memberService.updateMemberPoint(gp.longValue(), PointTypeEnum.INCREASE.name(), member.getId(),
                        "等级权益「" + benefit.getBenefitName() + "」赠送积分",
                        PointSourceEnum.BENEFIT.name())))
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
}
