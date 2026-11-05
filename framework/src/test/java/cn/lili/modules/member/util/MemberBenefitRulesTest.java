package cn.lili.modules.member.util;

import cn.hutool.json.JSONObject;
import cn.lili.common.exception.ServiceException;
import org.junit.jupiter.api.Test;
import java.math.BigDecimal;
import java.time.LocalDate;
import static org.assertj.core.api.Assertions.*;

/**
 * 权益配置规则测试，覆盖月末周期、资格日期边界、配置精度、旧配置兼容及富文本过滤。
 *
 * @author Bulbasaur
 * @since 2026-11-04
 */
class MemberBenefitRulesTest {
    /**
     * 验证每月31日发放在短月取月末，覆盖普通年份2月28日、闰年2月29日及到期前不发放。
     */
    @Test void monthlyUsesLastDayInShortMonths() {
        var config = MemberBenefitRules.config("{\"grantRule\":\"MONTHLY\",\"grantDay\":31}");
        assertThat(MemberBenefitRules.cycle(config, LocalDate.of(2026,1,1), LocalDate.of(2026,2,27))).isNull();
        assertThat(MemberBenefitRules.cycle(config, LocalDate.of(2026,1,1), LocalDate.of(2026,2,28))).isEqualTo("MONTH:2026-02");
        assertThat(MemberBenefitRules.cycle(config, LocalDate.of(2028,1,1), LocalDate.of(2028,2,29))).isEqualTo("MONTH:2028-02");
    }
    /**
     * 验证本月发放日早于客户取得等级的日期时不补发，下一月到期后才生成周期键。
     */
    @Test void monthlyDoesNotBackfillBeforeMemberReachedGrade() {
        var config = MemberBenefitRules.config("{\"grantRule\":\"MONTHLY\",\"grantDay\":5}");
        assertThat(MemberBenefitRules.cycle(config, LocalDate.of(2026,11,10), LocalDate.of(2026,11,15))).isNull();
        assertThat(MemberBenefitRules.cycle(config, LocalDate.of(2026,11,10), LocalDate.of(2026,12,5))).isEqualTo("MONTH:2026-12");
    }
    /**
     * 验证自然月周期始终以原始1月31日为锚点，首期立即生效，2月取月末，3月恢复31日而不漂移。
     */
    @Test void periodAnchorsToOriginalAnniversaryWithoutEndOfMonthDrift() {
        var config = MemberBenefitRules.config("{\"grantRule\":\"PERIOD\",\"periodMonths\":1}");
        var since=LocalDate.of(2026,1,31);
        assertThat(MemberBenefitRules.cycle(config,since,since)).isEqualTo("PERIOD:1:0");
        assertThat(MemberBenefitRules.cycle(config,since,LocalDate.of(2026,2,28))).isEqualTo("PERIOD:1:1");
        assertThat(MemberBenefitRules.cycle(config,since,LocalDate.of(2026,3,30))).isEqualTo("PERIOD:1:1");
        assertThat(MemberBenefitRules.cycle(config,since,LocalDate.of(2026,3,31))).isEqualTo("PERIOD:1:2");
    }
    /**
     * 验证未知发放类型、零月周期、小数发放日和小数赠送积分均被拒绝。
     */
    @Test void invalidScheduleAndFractionalCountsAreRejected() {
        for(String json : new String[]{"{\"grantRule\":\"INVALID\"}","{\"grantRule\":\"PERIOD\",\"periodMonths\":0}","{\"grantRule\":\"MONTHLY\",\"grantDay\":1.5}"}) {
            assertThatThrownBy(()->MemberBenefitRules.validateSchedule(MemberBenefitRules.config(json))).isInstanceOf(ServiceException.class);
        }
        assertThatThrownBy(()->MemberBenefitRules.number(new JSONObject("{\"giftPoint\":1.5}"),"giftPoint",BigDecimal.ZERO,BigDecimal.valueOf(99999),0)).isInstanceOf(ServiceException.class);
    }
    /**
     * 验证旧配置未填写发放规则时按ONCE处理，积分倍率允许合法小数，例如1.5倍。
     */
    @Test void acceptsLegacyOnceAndDecimalMultiplier() {
        assertThat(MemberBenefitRules.cycle(new JSONObject(),LocalDate.now(),LocalDate.now())).isEqualTo("ONCE");
        assertThat(MemberBenefitRules.number(new JSONObject("{\"multiplier\":1.5}"),"multiplier",BigDecimal.ONE,BigDecimal.valueOf(100),2)).isEqualByComparingTo("1.5");
    }
    /**
     * 验证权益富文本清理保留可展示正文，同时去除脚本标签、事件属性和javascript协议链接。
     */
    @Test void sanitizesExistingHtmlAtBuyerBoundary() {
        String safe=MemberBenefitRules.description("<p>会员权益</p><script>alert(1)</script><img src=x onerror=alert(2)><a href=javascript:alert(3)>链接</a>");
        assertThat(safe).contains("会员权益").doesNotContain("<script", "onerror", "javascript:");
    }
}
