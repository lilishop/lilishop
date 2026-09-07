package cn.lili.modules.distribution.serviceimpl;

import cn.lili.common.exception.ServiceException;
import cn.lili.modules.distribution.entity.dos.DistributionGrade;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.math.BigDecimal;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * 分销等级佣金比例按 0～1 小数校验。
 *
 * @author Mike
 * @date 2026-09-07
 */
class DistributionGradeServiceImplRateValidationTest {

    private DistributionGradeServiceImpl service;

    @BeforeEach
    void setUp() {
        service = new DistributionGradeServiceImpl();
    }

    @Test
    void validateGrade_acceptsDecimalWithinZeroToOne() {
        assertDoesNotThrow(() -> invokeValidateGrade(baseGrade("0.10", "0")));
        assertDoesNotThrow(() -> invokeValidateGrade(baseGrade("0.01", "0")));
        assertDoesNotThrow(() -> invokeValidateGrade(baseGrade("1", "0")));
    }

    @Test
    void validateGrade_rejectsUnmigratedPercentTen() {
        ServiceException ex = assertThrows(ServiceException.class,
                () -> invokeValidateGrade(baseGrade("10", "0")));
        assertTrue(ex.getMsg().contains("不能超过100%"));
    }

    @Test
    void validateGrade_rejectsNegative() {
        ServiceException ex = assertThrows(ServiceException.class,
                () -> invokeValidateGrade(baseGrade("-0.1", "0")));
        assertTrue(ex.getMsg().contains("不能超过100%"));
    }

    @Test
    void validateGrade_rejectsSingleGradeSumOverOne() {
        ServiceException ex = assertThrows(ServiceException.class,
                () -> invokeValidateGrade(baseGrade("0.60", "0.50")));
        assertTrue(ex.getMsg().contains("不能超过100%"));
    }

    @Test
    void validateCrossRates_allowsDefaultTenPercentPlusInvite() {
        assertDoesNotThrow(() -> service.validateCrossRates(List.of(
                baseGrade("0.10", "0"),
                higherGrade("0.15", "0.05"))));
    }

    @Test
    void validateCrossRates_rejectsMaxGoodsPlusMaxInviteOverOne() {
        ServiceException ex = assertThrows(ServiceException.class,
                () -> service.validateCrossRates(List.of(
                        baseGrade("0.80", "0"),
                        higherGrade("0.10", "0.30"))));
        assertTrue(ex.getMsg().contains("不能超过100%"));
    }

    private void invokeValidateGrade(DistributionGrade grade) {
        ReflectionTestUtils.invokeMethod(service, "validateGrade", grade);
    }

    private static DistributionGrade baseGrade(String goods, String invite) {
        DistributionGrade grade = new DistributionGrade();
        grade.setSortOrder(1);
        grade.setGradeName("普通分销员");
        grade.setGoodsCommissionRate(new BigDecimal(goods));
        grade.setInviteCommissionRate(new BigDecimal(invite));
        return grade;
    }

    private static DistributionGrade higherGrade(String goods, String invite) {
        DistributionGrade grade = new DistributionGrade();
        grade.setSortOrder(2);
        grade.setGradeName("金牌");
        grade.setGoodsCommissionRate(new BigDecimal(goods));
        grade.setInviteCommissionRate(new BigDecimal(invite));
        return grade;
    }
}
