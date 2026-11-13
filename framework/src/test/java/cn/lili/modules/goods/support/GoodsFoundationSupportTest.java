package cn.lili.modules.goods.support;

import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.mapper.GoodsMapper;
import cn.lili.modules.goods.mapper.GoodsSkuMapper;
import cn.lili.modules.goods.service.GoodsSkuService;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.*;

/**
 * 商品基础能力拆分回归测试，验证SKU缓存协议和商品库存汇总口径，不连接数据库。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
class GoodsFoundationSupportTest {
    private final GoodsSkuMapper skus = mock(GoodsSkuMapper.class);
    private final GoodsMapper goods = mock(GoodsMapper.class);
    private final Cache cache = mock(Cache.class);

    /** 初始化真实查询和更新条件所需的实体元数据。 */
    @BeforeAll
    static void initMetadata() {
        var assistant = new MapperBuilderAssistant(new MybatisConfiguration(), "");
        TableInfoHelper.initTableInfo(assistant, Goods.class);
        TableInfoHelper.initTableInfo(assistant, GoodsSku.class);
    }

    /** 验证SKU读取复用原缓存键，未命中时读取存储并回填原对象。 */
    @Test
    void cacheMissLoadsAndFillsOriginalSkuKey() {
        GoodsSku sku = new GoodsSku();
        when(skus.selectById("sku")).thenReturn(sku);
        var reads = new GoodsSkuReadSupport(skus, cache, new GoodsSkuSpecAssembler());
        assertThat(reads.getGoodsSkuByIdFromCache("sku")).isSameAs(sku);
        verify(cache).put(GoodsSkuService.getCacheKeys("sku"), sku);
    }

    /** 验证缓存命中时不再访问SKU存储。 */
    @Test
    void cacheHitDoesNotReadStorage() {
        GoodsSku sku = new GoodsSku();
        when(cache.get(GoodsSkuService.getCacheKeys("sku"))).thenReturn(sku);
        var reads = new GoodsSkuReadSupport(skus, cache, new GoodsSkuSpecAssembler());
        assertThat(reads.getGoodsSkuByIdFromCache("sku")).isSameAs(sku);
        verifyNoInteractions(skus);
    }

    /** 验证商品库存为所有SKU数量之和，空数量按零处理，并保留商品缓存失效和主键限定。 */
    @Test
    void aggregatePreservesNullQuantityAndCacheInvalidationRules() {
        GoodsSku first = new GoodsSku();
        first.setQuantity(4);
        GoodsSku second = new GoodsSku();
        second.setQuantity(3);
        GoodsSku empty = new GoodsSku();
        when(skus.selectList(any(Wrapper.class))).thenReturn(List.of(first, second, empty));
        var aggregates = new GoodsStockAggregateSupport(goods, skus, cache);

        aggregates.updateStock("goods");

        verify(cache).remove(CachePrefix.GOODS.getPrefix() + "goods");
        ArgumentCaptor<Wrapper<Goods>> update = ArgumentCaptor.forClass(Wrapper.class);
        verify(goods).update(isNull(), update.capture());
        assertThat(update.getValue().getSqlSegment()).contains("id");
        var wrapper = (com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper<Goods>) update.getValue();
        assertThat(wrapper.getSqlSet()).contains("quantity");
        assertThat(wrapper.getParamNameValuePairs()).containsValue("goods").containsValue(7);
    }
}
