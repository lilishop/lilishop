package cn.lili.modules.promotion.serviceimpl;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.order.order.mapper.OrderGiftCardRecordMapper;
import cn.lili.modules.promotion.entity.dto.search.GiftCardUsageRecordSearchParams;
import cn.lili.modules.promotion.entity.vos.GiftCardUsageRecordVO;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.test.util.ReflectionTestUtils;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

/**
 * 礼品卡使用记录查询范围测试，防止活动详情混入其他活动流水。
 *
 * @author Bulbasaur
 * @since 2026-11-06
 */
class GiftCardCashUsageRecordTest {

    /** 指定活动时必须按凭据所属活动 ID 精确筛选，且仍保留其他业务筛选条件。 */
    @Test
    void filtersByCredentialActivityAndKeepsOtherFilters() {
        GiftCardUsageRecordSearchParams params = new GiftCardUsageRecordSearchParams();
        params.setActivityId(" 2118585580174450688 ");
        params.setOrderSn("order-1");
        params.setTransactionType("ORDER_DEDUCT");

        Wrapper<GiftCardUsageRecordVO> wrapper = captureQuery(params);
        String sql = wrapper.getCustomSqlSegment();

        assertThat(sql).contains("c.activity_id =", "ogr.order_sn =", "ogr.biz_type =", "ogr.delete_flag =");
        assertThat(((com.baomidou.mybatisplus.core.conditions.query.QueryWrapper<?>) wrapper).getParamNameValuePairs().values())
                .contains("2118585580174450688", "order-1", "ORDER_DEDUCT", "PICKUP_CANCEL");
    }

    /** 未指定活动时保留已有全局查询兼容性，空白活动 ID 不生成错误筛选。 */
    @Test
    void globalQueryRemainsCompatibleWithoutActivity() {
        GiftCardUsageRecordSearchParams params = new GiftCardUsageRecordSearchParams();
        params.setActivityId(" ");
        assertThat(captureQuery(params).getCustomSqlSegment()).doesNotContain("c.activity_id");
        assertThat(captureQuery(null).getCustomSqlSegment()).doesNotContain("c.activity_id");
    }

    /**
     * 调用真实查询条件组装并捕获 Mapper 入参，不访问业务数据库。
     * @param params 查询条件，可为空
     * @return 传入 Mapper 的查询条件
     */
    @SuppressWarnings({"rawtypes", "unchecked"})
    private Wrapper<GiftCardUsageRecordVO> captureQuery(GiftCardUsageRecordSearchParams params) {
        GiftCardCashServiceImpl service = new GiftCardCashServiceImpl();
        OrderGiftCardRecordMapper mapper = mock(OrderGiftCardRecordMapper.class);
        ReflectionTestUtils.setField(service, "orderGiftCardRecordMapper", mapper);
        service.pageUsageRecord(params, new PageVO());
        ArgumentCaptor<Wrapper> query = ArgumentCaptor.forClass(Wrapper.class);
        verify(mapper).pageUsageRecord(any(), query.capture());
        return query.getValue();
    }
}
