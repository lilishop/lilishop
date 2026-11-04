package cn.lili.modules.promotion.serviceimpl;

import cn.lili.common.exception.ServiceException;
import cn.lili.modules.promotion.entity.vos.CouponVO;
import org.junit.jupiter.api.Test;

import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatCode;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/**
 * 优惠券优惠形式校验（{@link CouponServiceImpl#checkPromotions}）的单元测试。
 * 覆盖三种优惠形式：立减券（现金券 + 零门槛）、现金券（有门槛）、满折券。
 */
class CouponServiceImplBenefitTest {
    private final CouponServiceImpl service = new CouponServiceImpl();

    /**
     * 构造一张基准合法的优惠券：面值 20 元 / 8.5 折、限领 1 张、发行 100 张、
     * 动态有效期 7 天、全场通用。各用例在此基础上覆盖类型与门槛制造非法数据。
     *
     * @param type      优惠券类型（PRICE 现金券 / DISCOUNT 满折券）
     * @param threshold 消费门槛
     * @return 基准优惠券
     */
    private CouponVO coupon(String type, double threshold) {
        CouponVO coupon = new CouponVO();
        coupon.setCouponType(type);
        coupon.setConsumeThreshold(threshold);
        coupon.setPrice(20D);
        coupon.setCouponDiscount(8.5D);
        coupon.setCouponLimitNum(1);
        coupon.setPublishNum(100);
        coupon.setRangeDayType("DYNAMICTIME");
        coupon.setEffectiveDays(7);
        coupon.setScopeType("ALL");
        return coupon;
    }

    /**
     * 三种优惠形式（立减券、现金券、满折券）均可通过校验；
     * 校验后互斥字段被清空：现金券清空折扣，满折券清空面值，立减券门槛保持为 0。
     */
    @Test
    void acceptsAllThreeBenefitsAndClearsIrrelevantValues() {
        CouponVO direct = coupon("PRICE", 0);
        CouponVO full = coupon("PRICE", 100);
        CouponVO discount = coupon("DISCOUNT", 100);
        Stream.of(direct, full, discount).forEach(c ->
                assertThatCode(() -> service.checkPromotions(c)).doesNotThrowAnyException());
        assertThat(direct.getConsumeThreshold()).isZero();
        assertThat(direct.getCouponDiscount()).isNull();
        assertThat(full.getCouponDiscount()).isNull();
        assertThat(discount.getPrice()).isNull();
    }

    /**
     * 现金券金额与门槛的边界校验：面值必须大于 0、不超过 99999999、最多两位小数；
     * 门槛必须非空、不为负、同样受金额范围与精度限制。
     * null / 0 / 负数 / 三位小数 / 超限 / NaN / 无穷大均抛 {@link ServiceException}。
     */
    @Test
    void rejectsInvalidCashAmountsAndThresholds() {
        for (Double amount : new Double[]{null, 0D, -1D, 0.001D, 100000000D, Double.NaN, Double.POSITIVE_INFINITY}) {
            CouponVO coupon = coupon("PRICE", 0);
            coupon.setPrice(amount);
            assertThatThrownBy(() -> service.checkPromotions(coupon)).isInstanceOf(ServiceException.class);
        }
        for (Double threshold : new Double[]{null, -1D, 0.001D, 100000000D, Double.NaN}) {
            CouponVO coupon = coupon("PRICE", 0);
            coupon.setConsumeThreshold(threshold);
            assertThatThrownBy(() -> service.checkPromotions(coupon)).isInstanceOf(ServiceException.class);
        }
    }

    /**
     * 类型与满折规则校验：未知优惠券类型直接拒绝；满折券不允许零门槛；
     * 折扣必须在 0.1~9.9 之间且最多一位小数，null / 0 / 10 / 8.55 / NaN 均拒绝。
     */
    @Test
    void rejectsUnknownTypesAndInvalidFullDiscounts() {
        assertThatThrownBy(() -> service.checkPromotions(coupon("UNKNOWN", 0))).isInstanceOf(ServiceException.class);
        assertThatThrownBy(() -> service.checkPromotions(coupon("DISCOUNT", 0))).isInstanceOf(ServiceException.class);
        for (Double discount : new Double[]{null, 0D, 10D, 8.55D, Double.NaN}) {
            CouponVO coupon = coupon("DISCOUNT", 100);
            coupon.setCouponDiscount(discount);
            assertThatThrownBy(() -> service.checkPromotions(coupon)).isInstanceOf(ServiceException.class);
        }
    }
}
