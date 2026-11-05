package cn.lili.modules.member.util;

import cn.lili.modules.member.entity.dos.MemberGrade;
import cn.lili.modules.member.entity.enums.MemberBenefitTypeEnum;
import cn.lili.modules.order.cart.render.RenderStepStatement;
import org.junit.jupiter.api.Test;
import java.util.Arrays;
import static org.assertj.core.api.Assertions.*;

/**
 * 普通客户等级权益范围回归测试，确保移除等级折扣字段、折扣权益类型和旧等级折扣渲染步骤。
 *
 * @author Bulbasaur
 * @since 2026-11-04
 */
class MemberBenefitScopeTest {
    /**
     * 验证普通等级仅保留六类支持的权益，不再支持MEMBER_DISCOUNT类型及discountRate字段。
     */
    @Test void gradesAndBenefitTypesDoNotExposeDiscount() {
        assertThat(MemberBenefitTypeEnum.values()).hasSize(6);
        assertThat(MemberBenefitTypeEnum.isValid("MEMBER_DISCOUNT")).isFalse();
        assertThat(Arrays.stream(MemberGrade.class.getDeclaredFields()).map(java.lang.reflect.Field::getName))
                .doesNotContain("discountRate");
    }

    /**
     * 验证购物车、结算及建单链均不再包含旧普通等级折扣步骤MEMBER_BENEFIT。
     * 本测试不禁止独立的付费会员优惠步骤PAID_MEMBER，避免混淆两类业务范围。
     */
    @Test void cartCheckoutAndTradeDoNotCalculateMembershipDiscount() {
        for (var steps : new cn.lili.modules.order.cart.entity.enums.RenderStepEnums[][] {
                RenderStepStatement.cartRender, RenderStepStatement.checkedRender, RenderStepStatement.tradeRender,
                RenderStepStatement.checkedSingleRender, RenderStepStatement.singleTradeRender,
                RenderStepStatement.pintuanTradeRender}) {
            assertThat(Arrays.stream(steps).map(Enum::name)).doesNotContain("MEMBER_BENEFIT");
        }
    }
}
