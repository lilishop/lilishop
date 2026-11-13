package cn.lili.modules.member.support;

import cn.lili.common.exception.ServiceException;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dos.MemberBenefit;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.promotion.entity.dos.Coupon;
import cn.lili.modules.promotion.service.CouponService;
import cn.lili.modules.promotion.service.MemberCouponService;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.*;
import static org.mockito.Mockito.*;

/**
 * 权益支持类回归测试，覆盖配置数值精度、平台券归属及发放失败传播，不连接真实数据库。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
class MemberBenefitSupportTest {

    private final CouponService coupons = mock(CouponService.class);
    private final MemberService members = mock(MemberService.class);
    private final MemberCouponService memberCoupons = mock(MemberCouponService.class);
    private final MemberBenefitConfigSupport configs = new MemberBenefitConfigSupport(coupons);
    private final MemberBenefitGrantSupport grants = new MemberBenefitGrantSupport(members, memberCoupons);

    /**
     * 构造包含公共必填字段的权益，便于隔离类型专属配置校验。
     *
     * @param type 权益类型编码
     * @param config 权益配置JSON
     * @return 尚未保存的权益
     */
    private MemberBenefit benefit(String type, String config) {
        MemberBenefit benefit = new MemberBenefit();
        benefit.setBenefitName("测试权益");
        benefit.setBenefitType(type);
        benefit.setBenefitConfig(config);
        benefit.setBenefitState("OPEN");
        benefit.setBenefitSort(1);
        benefit.setBenefitLogo("logo.png");
        benefit.setBenefitDesc("<p>权益介绍</p><script>alert(1)</script>");
        return benefit;
    }

    /**
     * 构造已锁定的领取客户样本，用户名空白时领券使用昵称。
     *
     * @return 领取客户
     */
    private Member member() {
        Member member = new Member();
        member.setId("member");
        member.setNickName("昵称");
        return member;
    }

    /** 验证赠送积分整数边界可保存，越界及小数被后端拒绝。 */
    @Test
    void validatesGiftPointRangeAndPrecision() {
        for (String value : new String[]{"0", "99999"}) {
            MemberBenefit benefit = benefit("GIFT_POINT", "{\"giftPoint\":" + value + "}");
            configs.validateBenefit(benefit);
            assertThat(benefit.getBenefitDesc()).doesNotContain("<script>");
        }
        for (String value : new String[]{"-1", "100000", "0.5"}) {
            assertThatThrownBy(() -> configs.validateBenefit(
                    benefit("GIFT_POINT", "{\"giftPoint\":" + value + "}")))
                    .isInstanceOf(ServiceException.class);
        }
    }

    /** 验证积分倍率最多两位小数，拒绝舍入后才合法的配置。 */
    @Test
    void rejectsMultiplierOverflowAndExcessPrecision() {
        configs.validateBenefit(benefit("POINT_MULTIPLIER", "{\"multiplier\":1.25}"));
        for (String value : new String[]{"0.99", "100.01", "1.001"}) {
            assertThatThrownBy(() -> configs.validateBenefit(
                    benefit("POINT_MULTIPLIER", "{\"multiplier\":" + value + "}")))
                    .isInstanceOf(ServiceException.class);
        }
    }

    /** 验证券礼包仅可配置平台券，且数量精度、重复关联和周期都受服务端校验。 */
    @Test
    void validatesCouponOwnershipQuantityDuplicatesAndSchedule() {
        Coupon coupon = new Coupon();
        coupon.setStoreId("store");
        when(coupons.getById("coupon")).thenReturn(coupon);
        String item = "{\"couponId\":\"coupon\",\"quantity\":1}";
        String config = "{\"coupons\":[" + item + "]}";
        assertThatThrownBy(() -> configs.validateBenefit(benefit("COUPON_PACKAGE", config)))
                .isInstanceOf(ServiceException.class);
        coupon.setStoreId("0");
        configs.validateBenefit(benefit("COUPON_PACKAGE", config));
        for (String invalid : new String[]{
                config.replace("\"quantity\":1", "\"quantity\":1.5"),
                config.replace("\"quantity\":1", "\"quantity\":11"),
                "{\"coupons\":[" + item + "," + item + "]}",
                "{\"grantRule\":\"MONTHLY\",\"grantDay\":32,\"coupons\":[" + item + "]}"}) {
            assertThatThrownBy(() -> configs.validateBenefit(benefit("COUPON_PACKAGE", invalid)))
                    .isInstanceOf(ServiceException.class);
        }
    }

    /** 验证积分更新失败继续抛出异常，供账本服务回滚单项发放并记录失败。 */
    @Test
    void propagatesPointUpdateFailure() {
        when(members.updateMemberPoint(anyLong(), anyString(), anyString(), anyString(), anyString()))
                .thenReturn(false);
        assertThatThrownBy(() -> grants.grantBenefitToMember(member(), benefit("GIFT_POINT", "{\"giftPoint\":10}")))
                .isInstanceOf(IllegalStateException.class).hasMessage("积分更新失败");
        verifyNoInteractions(memberCoupons);
    }

    /** 验证零积分权益不写积分流水，券礼包逐份领取且领券失败向上传播。 */
    @Test
    void grantsCouponsIndividuallyAndPropagatesFailure() {
        grants.grantBenefitToMember(member(), benefit("GIFT_POINT", "{\"giftPoint\":0}"));
        verifyNoInteractions(members);
        RuntimeException failure = new IllegalStateException("领券失败");
        doNothing().doThrow(failure).when(memberCoupons).receiveCoupon("coupon", "member", "昵称");
        assertThatThrownBy(() -> grants.grantBenefitToMember(member(), benefit("COUPON_PACKAGE",
                "{\"coupons\":[{\"couponId\":\" coupon \",\"quantity\":3}]}")))
                .isSameAs(failure);
        verify(memberCoupons, times(2)).receiveCoupon("coupon", "member", "昵称");
    }

    /** 验证优惠券删除后展示仍保留原配置关联和数量，空白配置返回空明细。 */
    @Test
    void preservesCouponReferenceWhenCouponIsMissing() {
        MemberBenefitCouponDisplaySupport display = new MemberBenefitCouponDisplaySupport(coupons);
        var rows = display.buildCouponPackageDisplayItems(
                "{\"coupons\":[{\"couponId\":\" missing \",\"quantity\":2}]}");
        assertThat(rows).hasSize(1);
        assertThat(rows.getFirst().getCouponId()).isEqualTo("missing");
        assertThat(rows.getFirst().getQuantity()).isEqualTo(2);
        assertThat(rows.getFirst().getCouponName()).isNull();
        assertThat(display.buildCouponPackageDisplayItems(" ")).isEmpty();
    }
}
