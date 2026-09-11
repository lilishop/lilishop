package cn.lili.controller.distribution;

import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.distribution.entity.dos.DistributionGrade;
import cn.lili.modules.distribution.entity.vos.DistributionBuyerGradeListVO;
import cn.lili.modules.distribution.service.DistributionGradeService;
import cn.lili.modules.distribution.support.DistributionSettingSupport;
import cn.lili.modules.system.entity.dto.DistributionSetting;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * GET /grades 返回包装 VO：开关与完整等级列表。
 * 
 * @author Mike
 * @date 2026-09-11
 */
@ExtendWith(MockitoExtension.class)
class DistributionBuyerControllerGradeListTest {

    @InjectMocks
    private DistributionBuyerController controller;

    @Mock
    private DistributionGradeService gradeService;

    @Mock
    private DistributionSettingSupport distributionSettingSupport;

    @Test
    void gradeListWhenVisibleTrueKeepsRates() {
        DistributionSetting setting = DistributionSetting.defaultSetting();
        setting.setUpgradeConditionVisible(true);
        when(distributionSettingSupport.getSetting()).thenReturn(setting);
        when(gradeService.listGrades()).thenReturn(List.of(sampleGrade()));

        ResultMessage<DistributionBuyerGradeListVO> message = controller.gradeList();

        DistributionBuyerGradeListVO vo = message.getResult();
        assertThat(vo.getUpgradeConditionVisible()).isTrue();
        assertThat(vo.getGrades()).hasSize(1);
        assertThat(vo.getGrades().get(0).getGoodsCommissionRate()).isEqualByComparingTo("0.10");
        assertThat(vo.getGrades().get(0).getInviteCommissionRate()).isEqualByComparingTo("0.05");
        assertThat(vo.getGrades().get(0).getSalesThreshold()).isEqualByComparingTo("500.00");
        verify(gradeService).listGrades();
    }

    @Test
    void gradeListWhenVisibleFalseStillReturnsCompleteGrades() {
        DistributionSetting setting = DistributionSetting.defaultSetting();
        setting.setUpgradeConditionVisible(false);
        when(distributionSettingSupport.getSetting()).thenReturn(setting);
        when(gradeService.listGrades()).thenReturn(List.of(sampleGrade()));

        ResultMessage<DistributionBuyerGradeListVO> message = controller.gradeList();

        DistributionBuyerGradeListVO vo = message.getResult();
        assertThat(vo.getUpgradeConditionVisible()).isFalse();
        assertThat(vo.getGrades()).hasSize(1);
        assertThat(vo.getGrades().get(0).getGoodsCommissionRate()).isEqualByComparingTo("0.10");
        assertThat(vo.getGrades().get(0).getInviteCommissionRate()).isEqualByComparingTo("0.05");
        assertThat(vo.getGrades().get(0).getEnableSalesCondition()).isTrue();
    }

    @Test
    void gradeListWhenSettingFieldMissingDefaultsTrue() {
        DistributionSetting setting = new DistributionSetting();
        setting.setIsOpen(true);
        assertThat(setting.getUpgradeConditionVisible()).isNull();
        setting.applyDefaults();
        when(distributionSettingSupport.getSetting()).thenReturn(setting);
        when(gradeService.listGrades()).thenReturn(List.of(sampleGrade()));

        ResultMessage<DistributionBuyerGradeListVO> message = controller.gradeList();

        assertThat(message.getResult().getUpgradeConditionVisible()).isTrue();
        assertThat(message.getResult().getGrades()).hasSize(1);
    }

    private static DistributionGrade sampleGrade() {
        DistributionGrade grade = new DistributionGrade();
        grade.setGradeName("普通分销员");
        grade.setSortOrder(1);
        grade.setGoodsCommissionRate(new BigDecimal("0.10"));
        grade.setInviteCommissionRate(new BigDecimal("0.05"));
        grade.setSalesThreshold(new BigDecimal("500.00"));
        grade.setEnableSalesCondition(true);
        return grade;
    }
}
