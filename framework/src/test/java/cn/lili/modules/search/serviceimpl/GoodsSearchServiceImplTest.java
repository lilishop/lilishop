package cn.lili.modules.search.serviceimpl;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.goods.entity.dto.GoodsSearchParams;
import cn.lili.modules.goods.entity.dto.GoodsSkuDTO;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.promotion.service.PromotionService;
import cn.lili.modules.search.entity.dos.GoodsIndex;
import cn.lili.modules.search.entity.dto.GoodsSearchDTO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Date;
import java.util.List;
import java.util.Map;

import static cn.lili.modules.search.serviceimpl.GoodsSearchTestFixtures.captureWrapper;
import static cn.lili.modules.search.serviceimpl.GoodsSearchTestFixtures.fullDiscountPromotionMap;
import static cn.lili.modules.search.serviceimpl.GoodsSearchTestFixtures.sku;
import static cn.lili.modules.search.serviceimpl.GoodsSearchTestFixtures.skuDto;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 商品分页及按SKU查询回归测试，验证数据库分页、促销加载和结果映射；不连接真实数据库。
 */
class GoodsSearchServiceImplTest {

    /**
     * 验证商品分页搜索使用MySQL的SKU数据并返回商品索引结果。
     */
    @Test
    void searchGoodsByPageUsesMysqlSkuDtoListAndReturnsGoodsIndexes() {
        GoodsSkuService goodsSkuService = mock(GoodsSkuService.class);
        PromotionService promotionService = mock(PromotionService.class);
        Page<GoodsSkuDTO> skuPage = new Page<>();
        GoodsSkuDTO sku = skuDto("sku-1", "goods-1", "测试商品", "内存", "8G");
        Date spuCreateTime = new Date(1_700_000_000_000L);
        sku.setGoodsCreateTime(spuCreateTime);
        skuPage.setRecords(List.of(sku));
        skuPage.setTotal(1);
        when(goodsSkuService.getGoodsSkuDTOByPage(any(Page.class), any())).thenReturn(skuPage);
        when(promotionService.getGoodsSkuPromotionMap("store-1", "sku-1"))
                .thenReturn(fullDiscountPromotionMap("promo-1"));

        GoodsSearchServiceImpl service = new GoodsSearchServiceImpl(goodsSkuService);
        ReflectionTestUtils.setField(service, "promotionService", promotionService);
        GoodsSearchDTO searchDTO = new GoodsSearchDTO();
        searchDTO.setKeyword("测试");
        searchDTO.setCategoryId("cat-1");
        PageVO pageVO = new PageVO();
        pageVO.setPageNumber(1);
        pageVO.setPageSize(10);

        Page<GoodsIndex> result = service.searchGoodsByPage(searchDTO, pageVO);

        Assertions.assertEquals(1, result.getTotal());
        Assertions.assertEquals("sku-1", result.getRecords().get(0).getId());
        Assertions.assertEquals("测试商品", result.getRecords().get(0).getGoodsName());
        Assertions.assertEquals(spuCreateTime, result.getRecords().get(0).getReleaseTime());
        Assertions.assertTrue(result.getRecords().get(0).getPromotionMap().containsKey("FULL_DISCOUNT-promo-1"));

        ArgumentCaptor<Page<GoodsSkuDTO>> pageCaptor = ArgumentCaptor.forClass(Page.class);
        ArgumentCaptor<QueryWrapper<GoodsSkuDTO>> captor = ArgumentCaptor.forClass(QueryWrapper.class);
        verify(goodsSkuService).getGoodsSkuDTOByPage(pageCaptor.capture(), captor.capture());
        Assertions.assertEquals(1, pageCaptor.getValue().getCurrent());
        Assertions.assertEquals(10, pageCaptor.getValue().getSize());
        String sqlSegment = captor.getValue().getCustomSqlSegment();
        Assertions.assertTrue(sqlSegment.contains("gs.goods_name"));
        Assertions.assertTrue(sqlSegment.contains("gs.category_path"));
        Assertions.assertTrue(sqlSegment.contains("gs.market_enable"));
        Assertions.assertTrue(sqlSegment.contains("gs.auth_flag"));
        Assertions.assertTrue(sqlSegment.contains("(g.delete_flag IS NULL OR g.delete_flag = 0)"));
        Assertions.assertTrue(sqlSegment.contains("ORDER BY gs.id DESC"));
        Assertions.assertFalse(sqlSegment.contains("li_goods_search_param"));
    }

    /**
     * 验证MySQL搜索按商品索引参数筛选。
     */
    @Test
    void searchGoodsByPageFiltersByIndexedGoodsParamsInMysqlMode() {
        GoodsSkuService goodsSkuService = mock(GoodsSkuService.class);
        Page<GoodsSkuDTO> skuPage = new Page<>();
        skuPage.setRecords(List.of(skuDto("sku-memory-8g", "goods-8g", "8G商品", "内存", "8G")));
        when(goodsSkuService.getGoodsSkuDTOByPage(any(Page.class), any())).thenReturn(skuPage);

        GoodsSearchServiceImpl service = new GoodsSearchServiceImpl(goodsSkuService);
        GoodsSearchDTO searchDTO = new GoodsSearchDTO();
        searchDTO.setProp("内存_8G");
        PageVO pageVO = new PageVO();
        pageVO.setPageNumber(1);
        pageVO.setPageSize(10);

        service.searchGoodsByPage(searchDTO, pageVO);

        QueryWrapper<GoodsSkuDTO> wrapper = captureWrapper(goodsSkuService);
        String sql = wrapper.getCustomSqlSegment();
        Assertions.assertTrue(sql.contains("EXISTS"));
        Assertions.assertTrue(sql.contains("li_goods_search_param"));
        Assertions.assertTrue(sql.contains("p.goods_id = gs.goods_id"));
        Assertions.assertFalse(sql.contains("内存"));
        Assertions.assertFalse(sql.contains("8G"));
        Assertions.assertTrue(wrapper.getParamNameValuePairs().containsValue("内存"));
        Assertions.assertTrue(wrapper.getParamNameValuePairs().containsValue("8G"));
    }

    /**
     * 验证仅为当前页商品加载促销信息。
     */
    @Test
    void searchGoodsByPageOnlyLoadsPromotionForCurrentPage() {
        GoodsSkuService goodsSkuService = mock(GoodsSkuService.class);
        PromotionService promotionService = mock(PromotionService.class);
        Page<GoodsSkuDTO> skuPage = new Page<>();
        skuPage.setTotal(3);
        skuPage.setRecords(List.of(skuDto("sku-1", "goods-1", "商品1", "内存", "8G")));
        when(goodsSkuService.getGoodsSkuDTOByPage(any(Page.class), any())).thenReturn(skuPage);
        when(promotionService.getGoodsSkuPromotionMap(any(), any())).thenReturn(Map.of());

        GoodsSearchServiceImpl service = new GoodsSearchServiceImpl(goodsSkuService);
        ReflectionTestUtils.setField(service, "promotionService", promotionService);
        PageVO pageVO = new PageVO();
        pageVO.setPageNumber(1);
        pageVO.setPageSize(1);

        Page<GoodsIndex> result = service.searchGoodsByPage(new GoodsSearchDTO(), pageVO);

        Assertions.assertEquals(3, result.getTotal());
        Assertions.assertEquals(1, result.getRecords().size());
        Assertions.assertEquals("sku-1", result.getRecords().get(0).getId());
        verify(promotionService).getGoodsSkuPromotionMap("store-1", "sku-1");
        verify(promotionService, never()).getGoodsSkuPromotionMap("store-1", "sku-2");
        verify(promotionService, never()).getGoodsSkuPromotionMap("store-1", "sku-3");
    }

    /**
     * 验证下推分页限制页大小且不重复挂载分页排序项。
     */
    @Test
    void searchGoodsByPagePushdownCapsPageSizeAndDoesNotAttachPageOrderItems() {
        GoodsSkuService goodsSkuService = mock(GoodsSkuService.class);
        when(goodsSkuService.getGoodsSkuDTOByPage(any(Page.class), any())).thenReturn(new Page<>());
        GoodsSearchServiceImpl service = new GoodsSearchServiceImpl(goodsSkuService);

        PageVO oversized = new PageVO();
        oversized.setPageNumber(2);
        oversized.setPageSize(500);
        service.searchGoodsByPage(new GoodsSearchDTO(), oversized);
        Page<GoodsSkuDTO> captured = capturePage(goodsSkuService);
        Assertions.assertEquals(2, captured.getCurrent());
        Assertions.assertEquals(100, captured.getSize());
        Assertions.assertTrue(captured.orders() == null || captured.orders().isEmpty());

        PageVO tooSmall = new PageVO();
        tooSmall.setPageNumber(0);
        tooSmall.setPageSize(0);
        service.searchGoodsByPage(new GoodsSearchDTO(), tooSmall);
        ArgumentCaptor<Page<GoodsSkuDTO>> pageCaptor = ArgumentCaptor.forClass(Page.class);
        verify(goodsSkuService, org.mockito.Mockito.times(2)).getGoodsSkuDTOByPage(pageCaptor.capture(), any());
        Page<GoodsSkuDTO> second = pageCaptor.getAllValues().get(1);
        Assertions.assertEquals(1, second.getCurrent());
        Assertions.assertEquals(10, second.getSize());
    }

    /**
     * 验证下推分页使用Mapper总数而非当前内存列表长度。
     */
    @Test
    void searchGoodsByPagePushdownUsesMapperTotalInsteadOfMemorySize() {
        GoodsSkuService goodsSkuService = mock(GoodsSkuService.class);
        PromotionService promotionService = mock(PromotionService.class);
        Page<GoodsSkuDTO> skuPage = new Page<>();
        skuPage.setTotal(88);
        skuPage.setRecords(List.of(skuDto("sku-1", "goods-1", "商品1", "内存", "8G")));
        when(goodsSkuService.getGoodsSkuDTOByPage(any(Page.class), any())).thenReturn(skuPage);
        when(promotionService.getGoodsSkuPromotionMap(any(), any())).thenReturn(Map.of());

        GoodsSearchServiceImpl service = new GoodsSearchServiceImpl(goodsSkuService);
        ReflectionTestUtils.setField(service, "promotionService", promotionService);
        PageVO pageVO = new PageVO();
        pageVO.setPageNumber(1);
        pageVO.setPageSize(1);

        Page<GoodsIndex> result = service.searchGoodsByPage(new GoodsSearchDTO(), pageVO);

        Assertions.assertEquals(88, result.getTotal());
        Assertions.assertEquals(1, result.getRecords().size());
        verify(promotionService).getGoodsSkuPromotionMap("store-1", "sku-1");
    }

    /**
     * 验证下推搜索使用商品创建时间填充上架时间。
     */
    @Test
    void searchGoodsByPagePushdownSetsReleaseTimeFromGoodsCreateTime() {
        GoodsSkuService goodsSkuService = mock(GoodsSkuService.class);
        Page<GoodsSkuDTO> skuPage = new Page<>();
        GoodsSkuDTO sku = skuDto("sku-1", "goods-1", "商品1", "内存", "8G");
        Date spuCreateTime = new Date(1_650_000_000_000L);
        sku.setGoodsCreateTime(spuCreateTime);
        skuPage.setRecords(List.of(sku));
        when(goodsSkuService.getGoodsSkuDTOByPage(any(Page.class), any())).thenReturn(skuPage);

        GoodsSearchServiceImpl service = new GoodsSearchServiceImpl(goodsSkuService);
        Page<GoodsIndex> result = service.searchGoodsByPage(new GoodsSearchDTO(), new PageVO());

        Assertions.assertEquals(spuCreateTime, result.getRecords().get(0).getReleaseTime());
    }

    /**
     * 捕获商品搜索传递给SKU服务的分页对象。
     *
     * @param goodsSkuService 商品SKU服务
     * @return 商品搜索传递给SKU服务的分页对象
     */
    private Page<GoodsSkuDTO> capturePage(GoodsSkuService goodsSkuService) {
        ArgumentCaptor<Page<GoodsSkuDTO>> captor = ArgumentCaptor.forClass(Page.class);
        verify(goodsSkuService).getGoodsSkuDTOByPage(captor.capture(), any());
        return captor.getValue();
    }

    /**
     * 验证按SKU ID查询时由MySQL SKU列表构建商品索引。
     */
    @Test
    void getGoodsBySkuIdsReturnsIndexesFromMysqlSkuList() {
        GoodsSkuService goodsSkuService = mock(GoodsSkuService.class);
        when(goodsSkuService.getGoodsSkuByList(any(GoodsSearchParams.class))).thenReturn(List.of(sku("sku-2", "goods-2", "指定商品")));

        GoodsSearchServiceImpl service = new GoodsSearchServiceImpl(goodsSkuService);

        List<GoodsIndex> result = service.getGoodsBySkuIds(List.of("sku-2"), null);

        Assertions.assertEquals(1, result.size());
        Assertions.assertEquals("sku-2", result.get(0).getId());
        ArgumentCaptor<GoodsSearchParams> captor = ArgumentCaptor.forClass(GoodsSearchParams.class);
        verify(goodsSkuService).getGoodsSkuByList(captor.capture());
        Assertions.assertEquals(List.of("sku-2"), captor.getValue().getSkuIds());
    }
}
