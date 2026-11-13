package cn.lili.modules.goods.serviceimpl;

import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.support.GoodsStockAggregateSupport;
import cn.lili.modules.goods.support.GoodsSkuStockManager;
import cn.lili.modules.goods.support.CardKeyPoolSupport;
import cn.lili.modules.goods.mapper.CardKeyMapper;
import cn.lili.modules.goods.mapper.GoodsSkuMapper;
import org.springframework.test.util.ReflectionTestUtils;
import static org.mockito.ArgumentMatchers.any;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * syncSkuStock 应将卡池 COUNT 写入 SKU quantity（updateStockFromCardKeySync）。
 *
 * @author Mike
 * @date 2026-08-02
 */
@ExtendWith(MockitoExtension.class)
class CardKeyServiceImplSyncSkuStockTest {

    @InjectMocks
    private CardKeyServiceImpl cardKeyService;

    @Mock
    private GoodsSkuMapper goodsSkuMapper;
    @Mock
    private CardKeyMapper cards;
    @Mock
    private GoodsSkuStockManager stocks;

    @Mock
    private GoodsStockAggregateSupport goodsService;

    private CardKeyServiceImpl serviceSpy;

    /**
     * 初始化当前用例的模拟依赖及测试数据。
     */
    @BeforeEach
    void setUp() {
        serviceSpy = spy(cardKeyService);
        ReflectionTestUtils.setField(serviceSpy, "cardKeyPoolSupport",
                new CardKeyPoolSupport(cards, goodsSkuMapper, stocks, goodsService));
    }

    /**
     * 验证SKU库存按可用卡池数量同步。
     */
    @Test
    void syncSkuStockWritesQuantityFromPoolCount() {
        GoodsSku sku = new GoodsSku();
        sku.setId("sku-1");
        sku.setGoodsId("goods-1");
        when(goodsSkuMapper.selectById("sku-1")).thenReturn(sku);
        when(cards.selectCount(any())).thenReturn(42L);

        serviceSpy.syncSkuStock("sku-1");

        verify(stocks).updateStockFromCardKeySync("sku-1", 42);
        verify(goodsService).updateStock("goods-1");
    }
}
