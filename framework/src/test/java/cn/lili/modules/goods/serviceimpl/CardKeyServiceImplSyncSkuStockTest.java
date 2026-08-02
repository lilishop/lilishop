package cn.lili.modules.goods.serviceimpl;

import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.modules.goods.service.GoodsSkuService;
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
    private GoodsSkuService goodsSkuService;

    @Mock
    private GoodsService goodsService;

    private CardKeyServiceImpl serviceSpy;

    @BeforeEach
    void setUp() {
        serviceSpy = spy(cardKeyService);
    }

    @Test
    void syncSkuStockWritesQuantityFromPoolCount() {
        GoodsSku sku = new GoodsSku();
        sku.setId("sku-1");
        sku.setGoodsId("goods-1");
        when(goodsSkuService.getById("sku-1")).thenReturn(sku);
        doReturn(42).when(serviceSpy).countUnusedPoolStock("sku-1");

        serviceSpy.syncSkuStock("sku-1");

        verify(goodsSkuService).updateStockFromCardKeySync("sku-1", 42);
        verify(goodsService).updateStock("goods-1");
    }
}
