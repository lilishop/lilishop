package cn.lili.modules.member.util;
import cn.lili.modules.system.entity.dto.PaidMemberSetting;
import org.junit.jupiter.api.Test;
import java.math.BigDecimal;
import java.time.ZonedDateTime;
import java.util.Date;
import static org.assertj.core.api.Assertions.*;
/**
 * 付费会员自然月、金额精度及退款尾差校验。
 * @author Bulbasaur
 * @since 2026-11-05
 */
class PaidMemberRulesTest {
    /**
     * 转换上海时区的测试日期。
     *
     * @param value 不含时区的 ISO 本地日期时间
     * @return 对应时间点
     */
    private Date date(String value) { return Date.from(ZonedDateTime.parse(value + "+08:00[Asia/Shanghai]").toInstant()); }
    /**
     * 验证自然月计算兼容月末及闰年，目标月份无对应日期时取月末。
     */
    @Test void calendarMonthsHandleMonthEndsAndLeapYears() {
        assertThat(PaidMemberRules.endTime(date("2024-01-31T12:30:00"),1)).isEqualTo(date("2024-02-29T12:30:00"));
        assertThat(PaidMemberRules.endTime(date("2024-02-29T12:30:00"),12)).isEqualTo(date("2025-02-28T12:30:00"));
    }
    /**
     * 验证会员单价四舍五入到分，包括低价商品半分进位。
     */
    @Test void pricesRoundOncePerUnitToCents() {
        assertThat(PaidMemberRules.price(new BigDecimal("100"),new BigDecimal("8.8"))).isEqualByComparingTo("88.00");
        assertThat(PaidMemberRules.price(new BigDecimal("0.05"),new BigDecimal("9"))).isEqualByComparingTo("0.05");
    }
    /**
     * 验证分三次退款累计冲回原补贴，拒绝超过原购买数量的退款。
     */
    @Test void partialRefundsExactlyRecoverOriginalSubsidy() {
        BigDecimal first=PaidMemberRules.refundSubsidy(new BigDecimal("0.01"),3,0,1);
        BigDecimal second=PaidMemberRules.refundSubsidy(new BigDecimal("0.01"),3,1,1);
        BigDecimal third=PaidMemberRules.refundSubsidy(new BigDecimal("0.01"),3,2,1);
        assertThat(first.add(second).add(third)).isEqualByComparingTo("0.01");
        assertThatThrownBy(()->PaidMemberRules.refundSubsidy(BigDecimal.ONE,3,2,2)).isInstanceOf(RuntimeException.class);
    }
    /**
     * 验证默认配置合法，拒绝非法期限、价格精度及超过十折的折扣。
     */
    @Test void configurationRejectsInvalidMoneyDurationAndDiscount() {
        var c=new PaidMemberSetting();PaidMemberRules.validate(c);
        c.setMonths(0);assertThatThrownBy(()->PaidMemberRules.validate(c)).isInstanceOf(RuntimeException.class);c.setMonths(12);
        c.setPrice(new BigDecimal("0.001"));assertThatThrownBy(()->PaidMemberRules.validate(c)).isInstanceOf(RuntimeException.class);c.setPrice(BigDecimal.ONE);
        c.setDiscount(new BigDecimal("10.01"));assertThatThrownBy(()->PaidMemberRules.validate(c)).isInstanceOf(RuntimeException.class);
    }
    /**
     * 验证自定义商家范围与自定义折扣商品列表不得为空。
     */
    @Test void customScopesCannotBeEmpty() {
        var c=new PaidMemberSetting();c.setStoreScope("CUSTOM");assertThatThrownBy(()->PaidMemberRules.validate(c)).isInstanceOf(RuntimeException.class);
        c.setStoreScope("SELF");c.setDiscountMode("CUSTOM");assertThatThrownBy(()->PaidMemberRules.validate(c)).isInstanceOf(RuntimeException.class);
    }
    /**
     * 验证颜色格式及入口文案的非空、50 字长度限制。
     */
    @Test void entryDisplayRejectsInvalidColorsAndEmptyOrOverlongTitles() {
        var c=new PaidMemberSetting();
        c.setUnopenedFontColor("red");assertThatThrownBy(()->PaidMemberRules.validate(c)).isInstanceOf(RuntimeException.class);
        c.setUnopenedFontColor("#F4DFB9");c.setOpenedText(" ");assertThatThrownBy(()->PaidMemberRules.validate(c)).isInstanceOf(RuntimeException.class);
        c.setOpenedText("会".repeat(51));assertThatThrownBy(()->PaidMemberRules.validate(c)).isInstanceOf(RuntimeException.class);
        c.setOpenedText("我的付费会员");PaidMemberRules.validate(c);
    }
}
