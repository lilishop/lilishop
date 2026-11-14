package cn.lili.modules.stock.serviceimpl;

import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.mapper.GoodsSkuMapper;
import cn.lili.modules.stock.entity.dos.StockFlow;
import cn.lili.modules.stock.entity.params.StockFlowSearchParams;
import cn.lili.modules.stock.mapper.StockFlowMapper;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * 库存流水分页图片补充测试，覆盖历史快照保留、批量查询、缺失 SKU 和店铺隔离。
 *
 * @author Bulbasaur
 * @since 2026-11-14
 */
@ExtendWith(MockitoExtension.class)
class StockFlowServiceImplTest {

    @InjectMocks
    private StockFlowServiceImpl service;

    @Mock
    private StockFlowMapper stockFlowMapper;

    @Mock
    private GoodsSkuMapper goodsSkuMapper;

    /** 初始化 MyBatis 字段元数据并注入流水 Mapper，测试无需数据库。 */
    @BeforeEach
    void setUp() {
        TableInfoHelper.initTableInfo(new MapperBuilderAssistant(new MybatisConfiguration(), ""), StockFlow.class);
        TableInfoHelper.initTableInfo(new MapperBuilderAssistant(new MybatisConfiguration(), ""), GoodsSku.class);
        ReflectionTestUtils.setField(service, "baseMapper", stockFlowMapper);
    }

    /** 多条流水使用一次批量查图，商品删除不影响返回，历史名称和规格保持原样。 */
    @Test
    void pageEnrichesImagesWithoutChangingHistoricalSnapshots() {
        StockFlow first = flow("sku-1", "store-1");
        first.setGoodsName("历史商品名称");
        first.setSpecs("历史规格");
        StockFlow second = flow("sku-1", "store-1");
        StockFlow missing = flow("deleted-sku", "store-1");
        stubPage(List.of(first, second, missing));
        GoodsSku sku = new GoodsSku();
        sku.setId("sku-1");
        sku.setStoreId("store-1");
        sku.setGoodsName("当前商品名称");
        sku.setSpecs("当前规格");
        sku.setThumbnail("/current-thumbnail.png");
        when(goodsSkuMapper.selectList(any(Wrapper.class))).thenReturn(List.of(sku));

        assertThat(service.page(params()).getRecords()).hasSize(3);

        assertThat(first.getThumbnail()).isEqualTo("/current-thumbnail.png");
        assertThat(second.getThumbnail()).isEqualTo("/current-thumbnail.png");
        assertThat(missing.getThumbnail()).isNull();
        assertThat(first.getGoodsName()).isEqualTo("历史商品名称");
        assertThat(first.getSpecs()).isEqualTo("历史规格");
        ArgumentCaptor<Wrapper<GoodsSku>> captor = ArgumentCaptor.forClass(Wrapper.class);
        verify(goodsSkuMapper).selectList(captor.capture());
        LambdaQueryWrapper<GoodsSku> query = (LambdaQueryWrapper<GoodsSku>) captor.getValue();
        assertThat(query.getSqlSegment()).contains("store_id", " IN ");
        assertThat(query.getParamNameValuePairs().values()).containsExactlyInAnyOrder("sku-1", "deleted-sku", "store-1");
    }

    /** 即使查询结果包含其他店铺 SKU，也不得将其图片填入当前店铺流水。 */
    @Test
    void pageDoesNotExposeAnotherStoresImage() {
        StockFlow record = flow("sku-1", "store-1");
        stubPage(List.of(record));
        GoodsSku sku = new GoodsSku();
        sku.setId("sku-1");
        sku.setStoreId("other-store");
        sku.setThumbnail("/other-store.png");
        when(goodsSkuMapper.selectList(any(Wrapper.class))).thenReturn(List.of(sku));

        service.page(params());

        assertThat(record.getThumbnail()).isNull();
    }

    /** 空页无需查询商品表，避免无意义或无条件的查库。 */
    @Test
    void emptyPageSkipsSkuQuery() {
        stubPage(List.of());

        assertThat(service.page(params()).getRecords()).isEmpty();

        verifyNoInteractions(goodsSkuMapper);
    }

    /** 创建带店铺归属的历史流水。 */
    private StockFlow flow(String skuId, String storeId) {
        StockFlow flow = new StockFlow();
        flow.setSkuId(skuId);
        flow.setStoreId(storeId);
        return flow;
    }

    /** 模拟已完成筛选及分页的流水数据。 */
    private void stubPage(List<StockFlow> records) {
        Page<StockFlow> page = new Page<>(1, 10, records.size());
        page.setRecords(records);
        when(stockFlowMapper.selectPage(any(), any())).thenReturn(page);
    }

    /** 构造当前店铺分页条件。 */
    private StockFlowSearchParams params() {
        StockFlowSearchParams params = new StockFlowSearchParams();
        params.setStoreId("store-1");
        params.setPageNumber(1);
        params.setPageSize(10);
        return params;
    }
}
