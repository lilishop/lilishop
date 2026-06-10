package cn.lili.modules.search.serviceimpl;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsSearchParams;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.search.entity.dos.EsGoodsIndex;
import cn.lili.modules.search.entity.dto.EsGoodsSearchDTO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class EsGoodsSearchServiceNoopTest {

    @Test
    void searchGoodsByPageUsesMysqlSkuListAndReturnsGoodsIndexes() {
        GoodsSkuService goodsSkuService = mock(GoodsSkuService.class);
        when(goodsSkuService.getGoodsSkuByList(any(GoodsSearchParams.class))).thenReturn(List.of(sku("sku-1", "goods-1", "测试商品")));

        EsGoodsSearchServiceNoop service = new EsGoodsSearchServiceNoop(goodsSkuService);
        EsGoodsSearchDTO searchDTO = new EsGoodsSearchDTO();
        searchDTO.setKeyword("测试");
        searchDTO.setCategoryId("cat-1");
        PageVO pageVO = new PageVO();
        pageVO.setPageNumber(1);
        pageVO.setPageSize(10);

        Page<EsGoodsIndex> result = service.searchGoodsByPage(searchDTO, pageVO);

        Assertions.assertEquals(1, result.getTotal());
        Assertions.assertEquals("sku-1", result.getRecords().get(0).getId());
        Assertions.assertEquals("测试商品", result.getRecords().get(0).getGoodsName());

        ArgumentCaptor<GoodsSearchParams> captor = ArgumentCaptor.forClass(GoodsSearchParams.class);
        verify(goodsSkuService).getGoodsSkuByList(captor.capture());
        GoodsSearchParams params = captor.getValue();
        Assertions.assertEquals("测试", params.getGoodsName());
        Assertions.assertEquals("cat-1", params.getCategoryPath());
        Assertions.assertEquals(GoodsStatusEnum.UPPER.name(), params.getMarketEnable());
        Assertions.assertEquals(GoodsAuthEnum.PASS.name(), params.getAuthFlag());
    }

    @Test
    void getEsGoodsBySkuIdsReturnsIndexesFromMysqlSkuList() {
        GoodsSkuService goodsSkuService = mock(GoodsSkuService.class);
        when(goodsSkuService.getGoodsSkuByList(any(GoodsSearchParams.class))).thenReturn(List.of(sku("sku-2", "goods-2", "指定商品")));

        EsGoodsSearchServiceNoop service = new EsGoodsSearchServiceNoop(goodsSkuService);

        List<EsGoodsIndex> result = service.getEsGoodsBySkuIds(List.of("sku-2"), null);

        Assertions.assertEquals(1, result.size());
        Assertions.assertEquals("sku-2", result.get(0).getId());
        ArgumentCaptor<GoodsSearchParams> captor = ArgumentCaptor.forClass(GoodsSearchParams.class);
        verify(goodsSkuService).getGoodsSkuByList(captor.capture());
        Assertions.assertEquals(List.of("sku-2"), captor.getValue().getSkuIds());
    }

    private GoodsSku sku(String skuId, String goodsId, String goodsName) {
        GoodsSku sku = new GoodsSku();
        sku.setId(skuId);
        sku.setGoodsId(goodsId);
        sku.setGoodsName(goodsName);
        sku.setMarketEnable(GoodsStatusEnum.UPPER.name());
        sku.setAuthFlag(GoodsAuthEnum.PASS.name());
        sku.setPrice(99D);
        sku.setStoreId("store-1");
        sku.setStoreName("测试店铺");
        sku.setCategoryPath("cat-1");
        sku.setBrandId("brand-1");
        return sku;
    }
}
