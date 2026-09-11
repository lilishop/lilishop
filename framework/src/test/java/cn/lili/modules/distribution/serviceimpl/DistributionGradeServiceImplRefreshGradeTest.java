package cn.lili.modules.distribution.serviceimpl;

import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionGrade;
import cn.lili.modules.distribution.entity.dos.DistributionGradeHistory;
import cn.lili.modules.distribution.mapper.DistributionGradeHistoryMapper;
import cn.lili.modules.distribution.mapper.DistributionMapper;
import cn.lili.modules.distribution.support.DistributionGradeReferenceSupport;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.test.util.ReflectionTestUtils;

import java.math.BigDecimal;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 业绩达标时 refreshGrade 写入等级历史。
 *
 * @author Mike
 * @date 2026-09-07
 */
class DistributionGradeServiceImplRefreshGradeTest {

    private DistributionGradeServiceImpl service;
    private DistributionMapper distributionMapper;
    private DistributionGradeHistoryMapper gradeHistoryMapper;
    private DistributionGradeReferenceSupport gradeReferenceSupport;

    @BeforeEach
    void setUp() {
        service = spy(new DistributionGradeServiceImpl());
        distributionMapper = mock(DistributionMapper.class);
        gradeHistoryMapper = mock(DistributionGradeHistoryMapper.class);
        gradeReferenceSupport = mock(DistributionGradeReferenceSupport.class);
        ReflectionTestUtils.setField(service, "distributionMapper", distributionMapper);
        ReflectionTestUtils.setField(service, "gradeHistoryMapper", gradeHistoryMapper);
        ReflectionTestUtils.setField(service, "gradeReferenceSupport", gradeReferenceSupport);

        when(gradeReferenceSupport.formatGradeValue(any())).thenAnswer(invocation -> {
            Integer sort = invocation.getArgument(0);
            return sort == null ? null : String.valueOf(sort);
        });
        doNothing().when(gradeReferenceSupport).normalizeGradeReference(any());
    }

    /**
     * 销售额达标时写入等级历史并升级。
     */
    @Test
    void refreshGradeInsertsHistoryWhenSalesThresholdMatched() {
        Distribution distribution = new Distribution();
        distribution.setId("D1");
        distribution.setGradeId("1");
        distribution.setValidSalesAmount(new BigDecimal("1000.00"));
        when(distributionMapper.selectById("D1")).thenReturn(distribution);
        when(distributionMapper.updateById(any(Distribution.class))).thenReturn(1);

        DistributionGrade current = grade(1, "普通分销员", false, BigDecimal.ZERO);
        DistributionGrade target = grade(2, "银牌分销员", true, new BigDecimal("500.00"));
        doReturn(List.of(current, target)).when(service).listGrades();
        doReturn(current).when(service).resolveGrade("1");

        // 销售额 1000 达到银牌门槛 500
        service.refreshGrade("D1");

        ArgumentCaptor<DistributionGradeHistory> historyCaptor = ArgumentCaptor.forClass(DistributionGradeHistory.class);
        verify(gradeHistoryMapper).insert(historyCaptor.capture());
        DistributionGradeHistory history = historyCaptor.getValue();
        assertThat(history.getDistributionId()).isEqualTo("D1");
        assertThat(history.getPreviousGradeId()).isEqualTo("1");
        assertThat(history.getGradeId()).isEqualTo("2");
        assertThat(history.getGradeName()).isEqualTo("银牌分销员");
        assertThat(history.getGoodsCommissionRate()).isEqualByComparingTo("0.15");
        assertThat(history.getInviteCommissionRate()).isEqualByComparingTo("0.05");
        assertThat(history.getUpgradeTime()).isNotNull();
        assertThat(distribution.getGradeId()).isEqualTo("2");
    }

    /**
     * 未达标时不写历史、不升级。
     */
    @Test
    void refreshGradeDoesNotInsertWhenThresholdNotMet() {
        Distribution distribution = new Distribution();
        distribution.setId("D1");
        distribution.setGradeId("1");
        distribution.setValidSalesAmount(new BigDecimal("10.00"));
        when(distributionMapper.selectById("D1")).thenReturn(distribution);

        DistributionGrade current = grade(1, "普通分销员", false, BigDecimal.ZERO);
        DistributionGrade target = grade(2, "银牌分销员", true, new BigDecimal("500.00"));
        doReturn(List.of(current, target)).when(service).listGrades();
        doReturn(current).when(service).resolveGrade("1");

        // 销售额 10 未达门槛
        service.refreshGrade("D1");

        verify(gradeHistoryMapper, never()).insert(any(DistributionGradeHistory.class));
        assertThat(distribution.getGradeId()).isEqualTo("1");
    }

    /**
     * 构造测试用分销等级。
     */
    private static DistributionGrade grade(int sort, String name, boolean enableSales, BigDecimal salesThreshold) {
        DistributionGrade grade = new DistributionGrade();
        grade.setSortOrder(sort);
        grade.setGradeName(name);
        grade.setEnableSalesCondition(enableSales);
        grade.setSalesThreshold(salesThreshold);
        grade.setGoodsCommissionRate(sort == 1 ? new BigDecimal("0.10") : new BigDecimal("0.15"));
        grade.setInviteCommissionRate(sort == 1 ? BigDecimal.ZERO : new BigDecimal("0.05"));
        return grade;
    }
}
