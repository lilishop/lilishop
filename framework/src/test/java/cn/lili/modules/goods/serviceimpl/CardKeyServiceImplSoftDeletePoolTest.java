package cn.lili.modules.goods.serviceimpl;

import cn.lili.modules.goods.entity.dos.CardKey;
import cn.lili.modules.goods.entity.enums.CardKeyStatusEnum;
import cn.lili.modules.goods.mapper.CardKeyMapper;
import cn.lili.modules.goods.support.CardKeyPoolSupport;
import static org.mockito.Mockito.when;
import static org.mockito.ArgumentMatchers.isNull;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.test.util.ReflectionTestUtils;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

/**
 * TS-32 / SVC-05：SKU 删除时软删 UNUSED/VOIDED 卡池并 syncSkuStock。
 *
 * @author Mike
 * @date 2026-08-03
 */
class CardKeyServiceImplSoftDeletePoolTest {

    private static final String SKU_ID = "sku-ecoupon-1";

    private CardKeyServiceImpl cardKeyService;
    private CardKeyMapper mapper;
    private CardKeyPoolSupport pool;

    /**
     * 初始化当前测试使用的MyBatis-Plus实体元数据。
     */
    @BeforeAll
    static void initEntityCache() {
        MapperBuilderAssistant assistant = new MapperBuilderAssistant(new MybatisConfiguration(), "");
        TableInfoHelper.initTableInfo(assistant, CardKey.class);
    }

    /**
     * 初始化当前用例的模拟依赖及测试数据。
     */
    @BeforeEach
    void setUp() {
        cardKeyService = spy(new CardKeyServiceImpl());
        mapper = mock(CardKeyMapper.class);
        pool = spy(new CardKeyPoolSupport(mapper, null, null, null));
        ReflectionTestUtils.setField(cardKeyService, "cardKeyPoolSupport", pool);
        when(mapper.update(isNull(), any(Wrapper.class))).thenReturn(1);
        doNothing().when(pool).syncSkuStock(SKU_ID);
    }

    /** 软删 UNUSED/VOIDED 并校验 UPDATE 条件含 status、delete_flag。 */
    @Test
    void softDeletePoolOnSkuRemoveUpdatesUnusedAndVoidedThenSyncsStock() {
        @SuppressWarnings("unchecked")
        ArgumentCaptor<Wrapper<CardKey>> captor = ArgumentCaptor.forClass(Wrapper.class);

        cardKeyService.softDeletePoolOnSkuRemove(SKU_ID);

        verify(mapper).update(isNull(), captor.capture());
        verify(pool).syncSkuStock(SKU_ID);

        String sql = captor.getValue().getSqlSegment();
        assertThat(sql).contains("sku_id");
        assertThat(sql).contains("status");
        assertThat(sql).contains(CardKeyStatusEnum.UNUSED.name());
        assertThat(sql).contains(CardKeyStatusEnum.VOIDED.name());
        assertThat(sql).contains("delete_flag");
    }

    /** UPDATE 失败时仍应 syncSkuStock。 */
    @Test
    void softDeletePoolOnSkuRemoveAlwaysSyncsStock() {
        when(mapper.update(isNull(), any(Wrapper.class))).thenReturn(0);

        cardKeyService.softDeletePoolOnSkuRemove(SKU_ID);

        verify(pool).syncSkuStock(eq(SKU_ID));
    }
}
