package cn.lili.modules.distribution.serviceimpl;

import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionGradeHistory;
import cn.lili.modules.distribution.entity.dto.DistributionGradeHistorySearchParams;
import cn.lili.modules.distribution.entity.vos.DistributionGradeHistoryVO;
import cn.lili.modules.distribution.mapper.DistributionMapper;
import cn.lili.modules.distribution.service.DistributionGradeService;
import cn.lili.modules.member.service.MemberService;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

/**
 * 后台等级历史分页能读出 refreshGrade 写入的快照。
 *
 * @author Mike
 * @date 2026-09-07
 */
class DistributionGradeHistoryServiceImplHistoryPageTest {

    private DistributionGradeHistoryServiceImpl service;
    private DistributionMapper distributionMapper;
    private DistributionGradeService distributionGradeService;

    @BeforeAll
    static void initMybatisEntityCache() {
        MapperBuilderAssistant assistant = new MapperBuilderAssistant(new MybatisConfiguration(), "");
        TableInfoHelper.initTableInfo(assistant, DistributionGradeHistory.class);
        TableInfoHelper.initTableInfo(assistant, Distribution.class);
    }

    @BeforeEach
    void setUp() {
        service = spy(new DistributionGradeHistoryServiceImpl());
        distributionMapper = mock(DistributionMapper.class);
        distributionGradeService = mock(DistributionGradeService.class);
        ReflectionTestUtils.setField(service, "distributionMapper", distributionMapper);
        ReflectionTestUtils.setField(service, "memberService", mock(MemberService.class));
        ReflectionTestUtils.setField(service, "distributionGradeService", distributionGradeService);
    }

    /**
     * 分页能读出升级快照中的等级与佣金比例。
     */
    @Test
    void historyPageReturnsUpgradeSnapshot() {
        DistributionGradeHistory history = new DistributionGradeHistory();
        history.setId("H1");
        history.setDistributionId("D1");
        history.setPreviousGradeId("1");
        history.setGradeId("2");
        history.setGradeName("银牌分销员");
        history.setGoodsCommissionRate(new BigDecimal("0.1500"));
        history.setInviteCommissionRate(new BigDecimal("0.0500"));
        history.setUpgradeTime(new Date());

        Page<DistributionGradeHistory> page = new Page<>(1, 10, 1);
        page.setRecords(List.of(history));
        doReturn(page).when(service).page(any(), any());

        Distribution distribution = new Distribution();
        distribution.setId("D1");
        distribution.setMemberName("直推");
        when(distributionMapper.selectById("D1")).thenReturn(distribution);
        // 旧等级查不到时回退展示「等级值1」
        when(distributionGradeService.resolveGrade("1")).thenReturn(null);

        DistributionGradeHistorySearchParams params = new DistributionGradeHistorySearchParams();
        params.setPageNumber(1);
        params.setPageSize(10);

        // 读取 refreshGrade 写入的升级快照
        IPage<DistributionGradeHistoryVO> result = service.historyPage(params);

        assertThat(result.getTotal()).isEqualTo(1);
        assertThat(result.getRecords()).hasSize(1);
        DistributionGradeHistoryVO vo = result.getRecords().get(0);
        assertThat(vo.getDistributionId()).isEqualTo("D1");
        assertThat(vo.getMemberName()).isEqualTo("直推");
        assertThat(vo.getGradeId()).isEqualTo("2");
        assertThat(vo.getGradeName()).isEqualTo("银牌分销员");
        assertThat(vo.getPreviousGradeName()).isEqualTo("等级值1");
        assertThat(vo.getGoodsCommissionRate()).isEqualByComparingTo("0.1500");
    }
}
