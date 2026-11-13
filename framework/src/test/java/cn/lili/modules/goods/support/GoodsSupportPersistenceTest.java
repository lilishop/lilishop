package cn.lili.modules.goods.support;

import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsSkuStockDTO;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.entity.vos.GoodsVO;
import cn.lili.modules.goods.mapper.GoodsMapper;
import cn.lili.modules.goods.mapper.GoodsSkuMapper;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import cn.lili.modules.stock.entity.dto.StockFlowContext;
import cn.lili.modules.stock.entity.enums.StockFlowTypeEnum;
import cn.lili.modules.stock.event.StockFlowEvent;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.context.ApplicationEventPublisher;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.*;

/**
 * 商品支持类持久化入口回归测试，验证移除反向服务依赖后的缓存、预警及库存流水行为，不连接数据库。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
class GoodsSupportPersistenceTest {

    private final GoodsSkuMapper mapper = mock(GoodsSkuMapper.class);
    private final GoodsStockAggregateSupport goods = mock(GoodsStockAggregateSupport.class);
    private final PromotionGoodsService promotions = mock(PromotionGoodsService.class);
    private final Cache cache = mock(Cache.class);
    private final ApplicationEventPublisher publisher = mock(ApplicationEventPublisher.class);
    private final GoodsSkuStockManager manager = new GoodsSkuStockManager(mapper, goods, promotions, cache, publisher);

    /** 初始化SKU的MyBatis元数据，用于验证真实库存更新条件。 */
    @BeforeAll
    static void initMetadata() {
        TableInfoHelper.initTableInfo(new MapperBuilderAssistant(new MybatisConfiguration(), ""), GoodsSku.class);
    }

    /**
     * 构造库存为10的普通SKU。
     *
     * @return 已持久化SKU样本
     */
    private GoodsSku sku() {
        GoodsSku sku = new GoodsSku();
        sku.setId("sku");
        sku.setGoodsId("goods");
        sku.setQuantity(10);
        sku.setGoodsType(GoodsTypeEnum.PHYSICAL_GOODS.name());
        return sku;
    }

    /** 验证商品详情命中缓存时直接返回，不访问商品Mapper。 */
    @Test
    void cachedGoodsViewDoesNotReadMapper() {
        GoodsMapper goodsMapper = mock(GoodsMapper.class);
        GoodsVO cached = new GoodsVO();
        when(cache.get(CachePrefix.GOODS.getPrefix() + "goods")).thenReturn(cached);
        GoodsVOAssembler assembler = new GoodsVOAssembler(goodsMapper, null, null, null, null, cache);

        assertThat(assembler.getGoodsVO("goods")).isSameAs(cached);
        verifyNoInteractions(goodsMapper);
    }

    /** 验证预警更新通过Mapper写入后仍替换SKU缓存并清理商品详情缓存。 */
    @Test
    void alertUpdatePreservesSkuCacheReplacement() {
        GoodsSku sku = sku();
        when(mapper.selectById("sku")).thenReturn(sku);
        GoodsSkuStockDTO change = new GoodsSkuStockDTO();
        change.setSkuId("sku");
        change.setAlertQuantity(3);

        manager.updateAlertQuantity(change);

        assertThat(sku.getAlertQuantity()).isEqualTo(3);
        verify(mapper).updateById(sku);
        verify(cache).remove(CachePrefix.GOODS.getPrefix() + "goods");
        verify(cache).remove(GoodsSkuService.getCacheKeys("sku"));
        verify(cache).put(GoodsSkuService.getCacheKeys("sku"), sku);
    }

    /** 验证批量预警仍按整批写入，分别保留各SKU的新预警值并清理关联商品缓存。 */
    @Test
    void batchAlertUpdateUsesMapperBatchAndPreservesPerSkuValues() {
        GoodsSku first = sku();
        GoodsSku second = sku();
        second.setId("sku-2");
        when(mapper.selectById("sku")).thenReturn(first);
        when(mapper.selectById("sku-2")).thenReturn(second);
        GoodsSkuStockDTO firstChange = new GoodsSkuStockDTO();
        firstChange.setSkuId("sku");
        firstChange.setGoodsId("goods");
        firstChange.setAlertQuantity(3);
        GoodsSkuStockDTO secondChange = new GoodsSkuStockDTO();
        secondChange.setSkuId("sku-2");
        secondChange.setGoodsId("goods");
        secondChange.setAlertQuantity(8);
        when(mapper.queryStocks(any())).thenReturn(List.of(firstChange, secondChange));

        manager.batchUpdateAlertQuantity(List.of(firstChange, secondChange));

        assertThat(first.getAlertQuantity()).isEqualTo(3);
        assertThat(second.getAlertQuantity()).isEqualTo(8);
        verify(mapper).updateById(List.of(first, second));
        verify(mapper, never()).updateById(any(GoodsSku.class));
        verify(cache).remove(CachePrefix.GOODS.getPrefix() + "goods");
    }

    /** 验证缓存未命中的库存覆盖仍维护促销库存、多级缓存及带差额的库存流水。 */
    @Test
    void stockUpdatePreservesCachesPromotionAndFlow() {
        GoodsSku sku = sku();
        when(mapper.selectById("sku")).thenReturn(sku);
        when(mapper.update(isNull(), any(Wrapper.class))).thenReturn(1);

        manager.updateStock("sku", 7,
                StockFlowContext.of(StockFlowTypeEnum.MANUAL_ADJUST, "batch", "operator", "调整"));

        verify(cache).put(GoodsSkuService.getStockCacheKey("sku"), 7);
        verify(cache).remove(CachePrefix.GOODS.getPrefix() + "goods");
        verify(promotions).updatePromotionGoodsStock("sku", 7);
        ArgumentCaptor<StockFlowEvent> event = ArgumentCaptor.forClass(StockFlowEvent.class);
        verify(publisher).publishEvent(event.capture());
        var message = event.getValue().getMessages().getFirst();
        assertThat(message.getBeforeStock()).isEqualTo(10);
        assertThat(message.getAfterStock()).isEqualTo(7);
        assertThat(message.getQuantity()).isEqualTo(3);
        assertThat(message.getDirection()).isEqualTo("OUT");
        assertThat(message.getBizSn()).isEqualTo("batch");
    }
}
