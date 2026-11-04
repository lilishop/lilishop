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

    /**
     * 初始化当前用例的模拟依赖及测试数据。
     */
    @BeforeEach
    void setUp() {
        service = new DistributionGradeServiceImpl();
    }

    /**
     * 验证分销等级接受零到一范围内的佣金比例。
     */
    @Test
    void validateGrade_acceptsDecimalWithinZeroToOne() {
        assertDoesNotThrow(() -> invokeValidateGrade(baseGrade("0.10", "0")));
        assertDoesNotThrow(() -> invokeValidateGrade(baseGrade("0.01", "0")));
        assertDoesNotThrow(() -> invokeValidateGrade(baseGrade("1", "0")));
    }

    /**
     * 验证未转换为小数的百分比数值被拒绝。
     */
    @Test
    void validateGrade_rejectsUnmigratedPercentTen() {
        ServiceException ex = assertThrows(ServiceException.class,
                () -> invokeValidateGrade(baseGrade("10", "0")));
        assertTrue(ex.getMsg().contains("不能超过100%"));
    }

    /**
     * 验证分销佣金比例不能为负数。
     */
    @Test
    void validateGrade_rejectsNegative() {
        ServiceException ex = assertThrows(ServiceException.class,
                () -> invokeValidateGrade(baseGrade("-0.1", "0")));
        assertTrue(ex.getMsg().contains("不能超过100%"));
    }

    /**
     * 验证同一等级的佣金比例总和不能超过一。
     */
    @Test
    void validateGrade_rejectsSingleGradeSumOverOne() {
        ServiceException ex = assertThrows(ServiceException.class,
                () -> invokeValidateGrade(baseGrade("0.60", "0.50")));
        assertTrue(ex.getMsg().contains("不能超过100%"));
    }

    /**
     * 验证默认商品佣金与邀请奖励合计合法时允许配置。
     */
    @Test
    void validateCrossRates_allowsDefaultTenPercentPlusInvite() {
        assertDoesNotThrow(() -> service.validateCrossRates(List.of(
                baseGrade("0.10", "0"),
                higherGrade("0.15", "0.05"))));
    }

    /**
     * 验证最高商品佣金与最高邀请奖励合计超过一时拒绝配置。
     */
    @Test
    void validateCrossRates_rejectsMaxGoodsPlusMaxInviteOverOne() {
        ServiceException ex = assertThrows(ServiceException.class,
                () -> service.validateCrossRates(List.of(
                        baseGrade("0.80", "0"),
                        higherGrade("0.10", "0.30"))));
        assertTrue(ex.getMsg().contains("不能超过100%"));
    }

    /**
     * 通过反射调用分销等级内部校验方法。
     *
     * @param grade 分销等级
     */
    private void invokeValidateGrade(DistributionGrade grade) {
        ReflectionTestUtils.invokeMethod(service, "validateGrade", grade);
    }

    /**
     * 构造基础分销等级测试数据。
     *
     * @param goods  商品佣金比例
     * @param invite 邀请奖励比例
     * @return 基础分销等级测试数据
     */
    private static DistributionGrade baseGrade(String goods, String invite) {
        DistributionGrade grade = new DistributionGrade();
        grade.setSortOrder(1);
        grade.setGradeName("普通分销员");
        grade.setGoodsCommissionRate(new BigDecimal(goods));
        grade.setInviteCommissionRate(new BigDecimal(invite));
        return grade;
    }

    /**
     * 构造更高等级的分销测试数据。
     *
     * @param goods  商品佣金比例
     * @param invite 邀请奖励比例
     * @return 更高等级的分销测试数据
     */
    private static DistributionGrade higherGrade(String goods, String invite) {
        DistributionGrade grade = new DistributionGrade();
        grade.setSortOrder(2);
        grade.setGradeName("金牌");
        grade.setGoodsCommissionRate(new BigDecimal(goods));
        grade.setInviteCommissionRate(new BigDecimal(invite));
        return grade;
    }
}
