package cn.lili.controller.distribution;

import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.enums.DistributionStatusEnum;
import cn.lili.modules.distribution.service.DistributionService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * GET /distribution 清退透出脱敏对象，且不写库。
 *
 * @author Mike
 * @date 2026-09-09
 */
@ExtendWith(MockitoExtension.class)
class DistributionBuyerControllerGetDistributionTest {

    @InjectMocks
    private DistributionBuyerController controller;

    @Mock
    private DistributionService distributionService;

    @Test
    void getDistributionReturnsNullWhenNoRecord() {
        when(distributionService.getDistribution()).thenReturn(null);

        ResultMessage<Distribution> message = controller.getDistribution();

        verify(distributionService).checkDistributionSetting();
        verify(distributionService, never()).updateById(any(Distribution.class));
        assertThat(message.getResult()).isNull();
    }

    @Test
    void getDistributionReturnsPassRecordUnmasked() {
        Distribution pass = retreatedLikeRecord(DistributionStatusEnum.PASS);
        when(distributionService.getDistribution()).thenReturn(pass);

        ResultMessage<Distribution> message = controller.getDistribution();

        verify(distributionService, never()).updateById(any(Distribution.class));
        assertThat(message.getResult()).isSameAs(pass);
        assertThat(message.getResult().getCanRebate()).isEqualTo(88.5D);
    }

    @Test
    void getDistributionReturnsMaskedRetreatObjectWithoutPersisting() {
        Distribution retreated = retreatedLikeRecord(DistributionStatusEnum.RETREAT);
        when(distributionService.getDistribution()).thenReturn(retreated);

        ResultMessage<Distribution> message = controller.getDistribution();

        verify(distributionService).checkDistributionSetting();
        verify(distributionService, never()).isRetreated(any());
        verify(distributionService, never()).updateById(any(Distribution.class));

        Distribution view = message.getResult();
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
    }

    private static Distribution retreatedLikeRecord(DistributionStatusEnum status) {
        Distribution distribution = new Distribution();
        distribution.setId("D1");
        distribution.setMemberId("M1");
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
