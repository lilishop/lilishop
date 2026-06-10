package cn.lili.modules.search.serviceimpl;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsSkuDTO;
import cn.lili.modules.goods.entity.dto.GoodsSearchParams;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.search.entity.dos.EsGoodsRelatedInfo;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.search.entity.dos.EsGoodsIndex;
import cn.lili.modules.search.entity.dto.EsGoodsSearchDTO;
import cn.lili.modules.search.entity.dto.ParamOptions;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
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
    void searchGoodsByPageUsesMysqlSkuDtoListAndReturnsGoodsIndexes() {
        GoodsSkuService goodsSkuService = mock(GoodsSkuService.class);
        Page<GoodsSkuDTO> skuPage = new Page<>();
        skuPage.setRecords(List.of(skuDto("sku-1", "goods-1", "测试商品", "内存", "8G")));
        when(goodsSkuService.getGoodsSkuDTOByPage(any(Page.class), any())).thenReturn(skuPage);

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

        ArgumentCaptor<QueryWrapper<GoodsSkuDTO>> captor = ArgumentCaptor.forClass(QueryWrapper.class);
        verify(goodsSkuService).getGoodsSkuDTOByPage(any(Page.class), captor.capture());
        String sqlSegment = captor.getValue().getCustomSqlSegment();
        Assertions.assertTrue(sqlSegment.contains("gs.goods_name"));
        Assertions.assertTrue(sqlSegment.contains("gs.category_path"));
        Assertions.assertTrue(sqlSegment.contains("gs.market_enable"));
        Assertions.assertTrue(sqlSegment.contains("gs.auth_flag"));
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

    @Test
    void searchGoodsByPageFiltersByIndexedGoodsParamsInMysqlMode() {
        GoodsSkuService goodsSkuService = mock(GoodsSkuService.class);
        Page<GoodsSkuDTO> skuPage = new Page<>();
        skuPage.setRecords(List.of(
                skuDto("sku-memory-8g", "goods-8g", "8G商品", "内存", "8G"),
                skuDto("sku-memory-16g", "goods-16g", "16G商品", "内存", "16G")
        ));
        when(goodsSkuService.getGoodsSkuDTOByPage(any(Page.class), any())).thenReturn(skuPage);

        EsGoodsSearchServiceNoop service = new EsGoodsSearchServiceNoop(goodsSkuService);
        EsGoodsSearchDTO searchDTO = new EsGoodsSearchDTO();
        searchDTO.setProp("内存_8G");
        PageVO pageVO = new PageVO();
        pageVO.setPageNumber(1);
        pageVO.setPageSize(10);

        Page<EsGoodsIndex> result = service.searchGoodsByPage(searchDTO, pageVO);

        Assertions.assertEquals(1, result.getTotal());
        Assertions.assertEquals("sku-memory-8g", result.getRecords().get(0).getId());
    }

    @Test
    void getSelectorReturnsIndexedGoodsParamOptionsInMysqlMode() {
        GoodsSkuService goodsSkuService = mock(GoodsSkuService.class);
        Page<GoodsSkuDTO> skuPage = new Page<>();
        skuPage.setRecords(List.of(
                skuDto("sku-memory-8g", "goods-8g", "8G商品", "内存", "8G"),
                skuDto("sku-memory-16g", "goods-16g", "16G商品", "内存", "16G")
        ));
        when(goodsSkuService.getGoodsSkuDTOByPage(any(Page.class), any())).thenReturn(skuPage);

        EsGoodsSearchServiceNoop service = new EsGoodsSearchServiceNoop(goodsSkuService);

        EsGoodsRelatedInfo result = service.getSelector(new EsGoodsSearchDTO(), new PageVO());

        Assertions.assertEquals(1, result.getParamOptions().size());
        ParamOptions option = result.getParamOptions().get(0);
        Assertions.assertEquals("内存", option.getKey());
        Assertions.assertEquals(List.of("8G", "16G"), option.getValues());
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

    private GoodsSkuDTO skuDto(String skuId, String goodsId, String goodsName, String paramName, String paramValue) {
        GoodsSkuDTO sku = new GoodsSkuDTO();
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
        sku.setParams("""
                [{"goodsParamsItemDTOList":[{"paramName":"%s","paramValue":"%s","isIndex":1,"sort":1}]}]
                """.formatted(paramName, paramValue));
        return sku;
    }
}
