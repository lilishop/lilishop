package cn.lili.modules.distribution.util;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * 分销佣金计算工具单元测试
 *
 * @author Bulbasaur
 * @since 2026-08-04
 */
class DistributionCommissionCalculatorTest {

    /**
     * 一级商品佣金 = 佣金池 × 等级商品佣金比例（0～1 小数）。
     */
    @Test
    void calculateDirectCommission_appliesGradeRateToPool() {
        // 佣金池 1 元、等级比例 0.10（10%）→ 0.10 元
        BigDecimal commission = DistributionCommissionCalculator.calculateDirectCommission(
                BigDecimal.valueOf(1), new BigDecimal("0.10"));
        assertEquals(new BigDecimal("0.10"), commission);

        // 佣金池 100 元、等级比例 0.10（10%）→ 10.00 元（主推商品展示场景）
        BigDecimal featured = DistributionCommissionCalculator.calculateDirectCommission(
                BigDecimal.valueOf(100), new BigDecimal("0.10"));
        assertEquals(new BigDecimal("10.00"), featured);
    }

    /**
     * 1% 与 100% 均为合法小数：0.01 拿池的 1%，1 拿整池。
     */
    @Test
    void calculateDirectCommission_supportsOnePercentAndFullPool() {
        BigDecimal onePercent = DistributionCommissionCalculator.calculateDirectCommission(
                BigDecimal.valueOf(100), new BigDecimal("0.01"));
        assertEquals(new BigDecimal("1.00"), onePercent);

        BigDecimal fullPool = DistributionCommissionCalculator.calculateDirectCommission(
                BigDecimal.valueOf(100), BigDecimal.ONE);
        assertEquals(new BigDecimal("100.00"), fullPool);
    }

    /**
     * 未迁数的百分数（如 10）不再 ÷100，也不截成 1。
     */
    @Test
    void calculateDirectCommission_doesNotTreatTenAsTenPercent() {
        BigDecimal dirty = DistributionCommissionCalculator.calculateDirectCommission(
                BigDecimal.valueOf(100), BigDecimal.valueOf(10));
        assertEquals(new BigDecimal("1000.00"), dirty);
    }

    /**
     * null、负数按 0 处理，不参与结算。
     */
    @Test
    void calculateDirectCommission_treatsNullAndNegativeAsZero() {
        BigDecimal fromNull = DistributionCommissionCalculator.calculateDirectCommission(
                BigDecimal.valueOf(100), null);
        assertEquals(new BigDecimal("0.00"), fromNull);

        BigDecimal fromNegative = DistributionCommissionCalculator.calculateDirectCommission(
                BigDecimal.valueOf(100), new BigDecimal("-0.10"));
        assertEquals(new BigDecimal("0.00"), fromNegative);
    }

    /**
     * 一级与二级佣金分别按各自等级比例从同一佣金池拆分，互不影响。
     */
    @Test
    void splitPool_splitsDirectAndInviteIndependently() {
        BigDecimal[] split = DistributionCommissionCalculator.splitPool(
                BigDecimal.valueOf(1), new BigDecimal("0.10"), new BigDecimal("0.20"));

        assertEquals(new BigDecimal("0.10"), split[0]); // 一级 10%
        assertEquals(new BigDecimal("0.20"), split[1]); // 二级 20%
    }

    /**
     * 仅有一级比例时，二级佣金应为 0。
     */
    @Test
    void splitPool_directOnlyLeavesInviteZero() {
        BigDecimal[] split = DistributionCommissionCalculator.splitPool(
                BigDecimal.valueOf(1), new BigDecimal("0.20"), BigDecimal.ZERO);

        assertEquals(new BigDecimal("0.20"), split[0]);
        assertEquals(new BigDecimal("0.00"), split[1]);
    }

    /**
     * 佣金池随数量放大时，一二级佣金同比例放大。
     */
    @Test
    void splitPool_scalesWithQuantity() {
        // 佣金池 2 元（等价于单品佣金 × 数量 2）
        BigDecimal[] split = DistributionCommissionCalculator.splitPool(
                BigDecimal.valueOf(2), new BigDecimal("0.10"), new BigDecimal("0.20"));

        assertEquals(new BigDecimal("0.20"), split[0]);
        assertEquals(new BigDecimal("0.40"), split[1]);
    }
}
