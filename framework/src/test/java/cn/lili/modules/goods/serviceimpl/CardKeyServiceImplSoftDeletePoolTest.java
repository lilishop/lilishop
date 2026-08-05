package cn.lili.modules.goods.serviceimpl;

import cn.lili.modules.goods.entity.dos.CardKey;
import cn.lili.modules.goods.entity.enums.CardKeyStatusEnum;
import cn.lili.modules.goods.service.GoodsSkuService;
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
    private GoodsSkuService goodsSkuService;

    @BeforeAll
    static void initEntityCache() {
        MapperBuilderAssistant assistant = new MapperBuilderAssistant(new MybatisConfiguration(), "");
        TableInfoHelper.initTableInfo(assistant, CardKey.class);
    }

    @BeforeEach
    void setUp() {
        cardKeyService = spy(new CardKeyServiceImpl());
        goodsSkuService = mock(GoodsSkuService.class);
        ReflectionTestUtils.setField(cardKeyService, "goodsSkuService", goodsSkuService);
        doReturn(true).when(cardKeyService).update(any());
        doNothing().when(cardKeyService).syncSkuStock(SKU_ID);
    }

    /** 软删 UNUSED/VOIDED 并校验 UPDATE 条件含 status、delete_flag。 */
    @Test
    void softDeletePoolOnSkuRemoveUpdatesUnusedAndVoidedThenSyncsStock() {
        @SuppressWarnings("unchecked")
        ArgumentCaptor<Wrapper<CardKey>> captor = ArgumentCaptor.forClass(Wrapper.class);

        cardKeyService.softDeletePoolOnSkuRemove(SKU_ID);

        verify(cardKeyService).update(captor.capture());
        verify(cardKeyService).syncSkuStock(SKU_ID);

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
        doReturn(false).when(cardKeyService).update(any());

        cardKeyService.softDeletePoolOnSkuRemove(SKU_ID);

        verify(cardKeyService).syncSkuStock(eq(SKU_ID));
    }
}
