package cn.lili.modules.distribution.serviceimpl;

import cn.hutool.core.date.DateUtil;
import cn.lili.modules.distribution.entity.dos.DistributionVisitEvent;
import cn.lili.modules.distribution.mapper.DistributionVisitEventMapper;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Date;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 访问记录幂等键由服务端拼接，sku 不进入 eventId。
 *
 * @author Mike
 * @date 2026-09-11
 */
class DistributionShareEventServiceImplRecordGoodsVisitTest {

    private static final String VISITOR_ID = "550e8400-e29b-41d4-a716-446655440000";
    private static final String DISTRIBUTION_ID = "1371447876820271104";
    private static final String GOODS_ID = "1371447876820271105";
    private static final String SKU_ID = "1371447876820271106";

    private DistributionShareEventServiceImpl service;
    private DistributionVisitEventMapper visitEventMapper;

    @BeforeAll
    static void initMybatisEntityCache() {
        MapperBuilderAssistant assistant = new MapperBuilderAssistant(new MybatisConfiguration(), "");
        TableInfoHelper.initTableInfo(assistant, DistributionVisitEvent.class);
    }

    @BeforeEach
    void setUp() {
        service = new DistributionShareEventServiceImpl();
        visitEventMapper = mock(DistributionVisitEventMapper.class);
        ReflectionTestUtils.setField(service, "visitEventMapper", visitEventMapper);
        when(visitEventMapper.selectCount(any())).thenReturn(0L);
        when(visitEventMapper.insert(any(DistributionVisitEvent.class))).thenReturn(1);
    }

    @Test
    void recordGoodsVisit_writesConcatenatedEventId() {
        service.recordGoodsVisit(DISTRIBUTION_ID, GOODS_ID, SKU_ID, null, VISITOR_ID, "M1");

        ArgumentCaptor<DistributionVisitEvent> captor = ArgumentCaptor.forClass(DistributionVisitEvent.class);
        verify(visitEventMapper).insert(captor.capture());
        String expected = VISITOR_ID + ":" + DISTRIBUTION_ID + ":" + GOODS_ID + ":" + DateUtil.formatDate(new Date());
        assertThat(VISITOR_ID).hasSize(36);
        assertThat(DISTRIBUTION_ID).hasSize(19);
        assertThat(GOODS_ID).hasSize(19);
        assertThat(captor.getValue().getEventId()).isEqualTo(expected);
    }

    @Test
    void recordGoodsVisit_skipsInsertWhenEventIdExists() {
        when(visitEventMapper.selectCount(any())).thenReturn(1L);

        service.recordGoodsVisit(DISTRIBUTION_ID, GOODS_ID, SKU_ID, null, VISITOR_ID, "M1");

        verify(visitEventMapper, never()).insert(any(DistributionVisitEvent.class));
    }

    @Test
    void recordGoodsVisit_skipsWhenSkuIdBlank() {
        service.recordGoodsVisit(DISTRIBUTION_ID, GOODS_ID, "", null, VISITOR_ID, "M1");
        service.recordGoodsVisit(DISTRIBUTION_ID, GOODS_ID, "  ", null, VISITOR_ID, "M1");
        service.recordGoodsVisit(DISTRIBUTION_ID, GOODS_ID, null, null, VISITOR_ID, "M1");

        verify(visitEventMapper, never()).selectCount(any());
        verify(visitEventMapper, never()).insert(any(DistributionVisitEvent.class));
    }

    @Test
    void recordGoodsVisit_sameGoodsDifferentSkuSharesEventId() {
        service.recordGoodsVisit(DISTRIBUTION_ID, GOODS_ID, SKU_ID, null, VISITOR_ID, "M1");
        service.recordGoodsVisit(DISTRIBUTION_ID, GOODS_ID, "1371447876820271199", null, VISITOR_ID, "M1");

        ArgumentCaptor<DistributionVisitEvent> captor = ArgumentCaptor.forClass(DistributionVisitEvent.class);
        verify(visitEventMapper, times(2)).insert(captor.capture());
        assertThat(captor.getAllValues().get(0).getEventId())
                .isEqualTo(captor.getAllValues().get(1).getEventId());
    }
}
