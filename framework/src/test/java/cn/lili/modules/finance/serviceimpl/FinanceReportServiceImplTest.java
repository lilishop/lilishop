package cn.lili.modules.finance.serviceimpl;

import cn.lili.modules.finance.entity.dto.FinanceReportSearchParams;
import cn.lili.modules.finance.entity.vo.PlatformPeriodReportVO;
import cn.lili.modules.finance.mapper.FinanceReportMapper;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Date;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * {@link FinanceReportServiceImpl} 单元测试。
 * <p>
 * 使用 Mock Mapper 验证时间范围解析及报表查询委托逻辑，不依赖数据库。
 */
@ExtendWith(MockitoExtension.class)
class FinanceReportServiceImplTest {

    @Mock
    private FinanceReportMapper financeReportMapper;

    @InjectMocks
    private FinanceReportServiceImpl financeReportService;

    @Test
    void platformPeriodReport_delegatesToMapper() {
        PlatformPeriodReportVO row = new PlatformPeriodReportVO();
        row.setPeriod("2026-06-01");
        row.setGmv(1000D);
        row.setRefundAmount(100D);
        row.setNetGmv(900D);
        row.setStoreSettlementTotal(800D);

        when(financeReportMapper.platformPeriodReport(any(Date.class), any(Date.class), isNull(), eq("%Y-%m-%d")))
                .thenReturn(List.of(row));

        FinanceReportSearchParams params = new FinanceReportSearchParams();
        params.setStartDate("2026-06-01");
        params.setEndDate("2026-06-07");
        params.setGranularity(FinanceReportSearchParams.GRANULARITY_DAY);

        List<PlatformPeriodReportVO> result = financeReportService.platformPeriodReport(params);

        assertNotNull(result);
        assertEquals(1, result.size());
        assertEquals(900D, result.get(0).getNetGmv());
        verify(financeReportMapper).platformPeriodReport(any(Date.class), any(Date.class), isNull(), eq("%Y-%m-%d"));
    }
}
