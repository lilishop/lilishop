package cn.lili.modules.promotion.serviceimpl;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.promotion.entity.dto.search.GiftCardCashCreateBatchSearchParams;
import cn.lili.modules.promotion.entity.dto.search.GiftCardCashIssueBatchSearchParams;
import cn.lili.modules.promotion.entity.vos.GiftCardCashCreateBatchVO;
import cn.lili.modules.promotion.entity.vos.GiftCardCashIssueBatchVO;
import cn.lili.modules.promotion.mapper.GiftCardCashCreateBatchMapper;
import cn.lili.modules.promotion.mapper.GiftCardCashIssueBatchMapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Date;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

/**
 * 礼品卡制卡批次与发卡批次分页查询条件参数化测试，验证子查询不存在原生 SQL 拼接与注入隐患。
 *
 * @author Bulbasaur
 * @since 2026-11-11
 */
class GiftCardCashBatchQueryTest {

    /**
     * 验证制卡批次查询包含卡号（含特殊字符/注入载荷）和过期时间时，必须使用参数化占位符绑定，杜绝原生 SQL 字符串拼接。
     */
    @Test
    @SuppressWarnings({"rawtypes", "unchecked"})
    void createBatchParameterizedQueryPreventsSqlInjection() {
        GiftCardCashServiceImpl service = new GiftCardCashServiceImpl();
        GiftCardCashCreateBatchMapper mapper = mock(GiftCardCashCreateBatchMapper.class);
        ReflectionTestUtils.setField(service, "createBatchMapper", mapper);

        String maliciousCardNo = "TEST_CARD' OR '1'='1 -- \\";
        Date start = new Date(1700000000000L);
        Date end = new Date(1730000000000L);

        GiftCardCashCreateBatchSearchParams params = new GiftCardCashCreateBatchSearchParams();
        params.setCardNo(maliciousCardNo);
        params.setExpireTimeStart(start);
        params.setExpireTimeEnd(end);

        service.pageCreateBatch(params, new PageVO());

        ArgumentCaptor<QueryWrapper<GiftCardCashCreateBatchVO>> captor = ArgumentCaptor.forClass(QueryWrapper.class);
        verify(mapper).pageByParams(any(), captor.capture());
        QueryWrapper<GiftCardCashCreateBatchVO> wrapper = captor.getValue();

        String sql = wrapper.getCustomSqlSegment();
        // 原生 SQL 中不得直接出现未转义的恶意载荷字面量
        assertThat(sql).doesNotContain(maliciousCardNo);
        assertThat(sql).contains("EXISTS (SELECT 1 FROM li_gift_card_cash_credential c WHERE c.delete_flag = 0 AND c.create_batch_id = cb.id");
        assertThat(sql).contains("c.card_no LIKE");
        assertThat(sql).contains("c.expire_time >=");
        assertThat(sql).contains("c.expire_time <=");

        // 参数必须通过 MyBatis-Plus 参数映射传递
        assertThat(wrapper.getParamNameValuePairs().values())
                .contains("%" + maliciousCardNo + "%", start, end);
    }

    /**
     * 验证制卡批次未提供卡号与过期时间条件时，不生成多余的凭证 EXISTS 子查询。
     */
    @Test
    @SuppressWarnings({"rawtypes", "unchecked"})
    void createBatchWithoutCredentialFiltersOmitsExistsSubquery() {
        GiftCardCashServiceImpl service = new GiftCardCashServiceImpl();
        GiftCardCashCreateBatchMapper mapper = mock(GiftCardCashCreateBatchMapper.class);
        ReflectionTestUtils.setField(service, "createBatchMapper", mapper);

        GiftCardCashCreateBatchSearchParams params = new GiftCardCashCreateBatchSearchParams();
        params.setBatchRemark("测试批次");

        service.pageCreateBatch(params, new PageVO());

        ArgumentCaptor<QueryWrapper<GiftCardCashCreateBatchVO>> captor = ArgumentCaptor.forClass(QueryWrapper.class);
        verify(mapper).pageByParams(any(), captor.capture());
        QueryWrapper<GiftCardCashCreateBatchVO> wrapper = captor.getValue();

        assertThat(wrapper.getCustomSqlSegment()).doesNotContain("EXISTS");
        assertThat(wrapper.getParamNameValuePairs().values()).contains("%测试批次%");
    }

    /**
     * 验证发卡批次查询包含卡号与过期时间时，生成基于发卡批次 ID 关联的参数化 EXISTS 查询。
     */
    @Test
    @SuppressWarnings({"rawtypes", "unchecked"})
    void issueBatchParameterizedQueryPreventsSqlInjection() {
        GiftCardCashServiceImpl service = new GiftCardCashServiceImpl();
        GiftCardCashIssueBatchMapper mapper = mock(GiftCardCashIssueBatchMapper.class);
        ReflectionTestUtils.setField(service, "issueBatchMapper", mapper);

        String cardNo = "ISSUE_CARD_999";
        Date start = new Date(1700000000000L);

        GiftCardCashIssueBatchSearchParams params = new GiftCardCashIssueBatchSearchParams();
        params.setCardNo(cardNo);
        params.setExpireTimeStart(start);

        service.pageIssueBatch(params, new PageVO());

        ArgumentCaptor<QueryWrapper<GiftCardCashIssueBatchVO>> captor = ArgumentCaptor.forClass(QueryWrapper.class);
        verify(mapper).pageByParams(any(), captor.capture());
        QueryWrapper<GiftCardCashIssueBatchVO> wrapper = captor.getValue();

        String sql = wrapper.getCustomSqlSegment();
        assertThat(sql).doesNotContain(cardNo);
        assertThat(sql).contains("EXISTS (SELECT 1 FROM li_gift_card_cash_credential c WHERE c.delete_flag = 0 AND c.issue_batch_id = ib.id");
        assertThat(sql).contains("c.card_no LIKE");
        assertThat(sql).contains("c.expire_time >=");
        assertThat(sql).doesNotContain("c.expire_time <=");

        assertThat(wrapper.getParamNameValuePairs().values())
                .contains("%" + cardNo + "%", start);
    }
}
