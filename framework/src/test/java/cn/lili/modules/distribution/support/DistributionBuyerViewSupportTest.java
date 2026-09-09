package cn.lili.modules.distribution.support;

import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.enums.DistributionStatusEnum;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 买家 GET /distribution 清退脱敏：返回拷贝、不改原对象。
 *
 * @author Mike
 * @date 2026-09-09
 */
class DistributionBuyerViewSupportTest {

    @Test
    void forBuyerResponseReturnsNullWhenNoRecord() {
        assertThat(DistributionBuyerViewSupport.forBuyerResponse(null)).isNull();
    }

    @Test
    void forBuyerResponseKeepsPassInstanceUnchanged() {
        Distribution pass = activeRecord(DistributionStatusEnum.PASS);

        Distribution view = DistributionBuyerViewSupport.forBuyerResponse(pass);

        assertThat(view).isSameAs(pass);
        assertThat(view.getCanRebate()).isEqualTo(88.5D);
        assertThat(view.getRebateTotal()).isEqualTo(200D);
    }

    @Test
    void forBuyerResponseKeepsApplyAndRefuseUnmasked() {
        Distribution apply = activeRecord(DistributionStatusEnum.APPLY);
        Distribution refuse = activeRecord(DistributionStatusEnum.REFUSE);

        assertThat(DistributionBuyerViewSupport.forBuyerResponse(apply)).isSameAs(apply);
        assertThat(DistributionBuyerViewSupport.forBuyerResponse(refuse)).isSameAs(refuse);
        assertThat(apply.getCanRebate()).isEqualTo(88.5D);
        assertThat(refuse.getWithdrawnTotal()).isEqualByComparingTo("12.30");
    }

    @Test
    void forBuyerResponseMasksRetreatCopyAndLeavesSourceIntact() {
        Distribution retreated = activeRecord(DistributionStatusEnum.RETREAT);

        Distribution view = DistributionBuyerViewSupport.forBuyerResponse(retreated);

        assertThat(view).isNotNull();
        assertThat(view).isNotSameAs(retreated);
        assertThat(view.getId()).isEqualTo("D1");
        assertThat(view.getMemberId()).isEqualTo("M1");
        assertThat(view.getDistributionStatus()).isEqualTo(DistributionStatusEnum.RETREAT.name());
        assertThat(view.getCanRebate()).isEqualTo(0D);
        assertThat(view.getCommissionFrozen()).isEqualTo(0D);
        assertThat(view.getRebateTotal()).isEqualTo(0D);
        assertThat(view.getWithdrawalFrozen()).isEqualByComparingTo(BigDecimal.ZERO);
        assertThat(view.getWithdrawnTotal()).isEqualByComparingTo(BigDecimal.ZERO);
        assertThat(view.getDistributionOrderCount()).isZero();
        assertThat(view.getDistributionOrderPrice()).isEqualTo(0D);
        assertThat(view.getValidSalesAmount()).isEqualByComparingTo(BigDecimal.ZERO);
        assertThat(view.getValidCommissionAmount()).isEqualByComparingTo(BigDecimal.ZERO);
        assertThat(view.getValidCustomerCount()).isZero();
        assertThat(view.getInviteCount()).isZero();

        assertThat(retreated.getCanRebate()).isEqualTo(88.5D);
        assertThat(retreated.getCommissionFrozen()).isEqualTo(15D);
        assertThat(retreated.getRebateTotal()).isEqualTo(200D);
        assertThat(retreated.getWithdrawalFrozen()).isEqualByComparingTo("3.20");
        assertThat(retreated.getWithdrawnTotal()).isEqualByComparingTo("12.30");
        assertThat(retreated.getDistributionOrderCount()).isEqualTo(9);
        assertThat(retreated.getDistributionOrderPrice()).isEqualTo(500D);
        assertThat(retreated.getValidSalesAmount()).isEqualByComparingTo("1000.00");
        assertThat(retreated.getValidCommissionAmount()).isEqualByComparingTo("80.00");
        assertThat(retreated.getValidCustomerCount()).isEqualTo(4);
        assertThat(retreated.getInviteCount()).isEqualTo(2);
    }

    private static Distribution activeRecord(DistributionStatusEnum status) {
        Distribution distribution = new Distribution();
        distribution.setId("D1");
        distribution.setMemberId("M1");
        distribution.setMemberName("张三");
        distribution.setDistributionStatus(status.name());
        distribution.setCanRebate(88.5D);
        distribution.setCommissionFrozen(15D);
        distribution.setRebateTotal(200D);
        distribution.setWithdrawalFrozen(new BigDecimal("3.20"));
        distribution.setWithdrawnTotal(new BigDecimal("12.30"));
        distribution.setDistributionOrderCount(9);
        distribution.setDistributionOrderPrice(500D);
        distribution.setValidSalesAmount(new BigDecimal("1000.00"));
        distribution.setValidCommissionAmount(new BigDecimal("80.00"));
        distribution.setValidCustomerCount(4);
        distribution.setInviteCount(2);
        return distribution;
    }
}
