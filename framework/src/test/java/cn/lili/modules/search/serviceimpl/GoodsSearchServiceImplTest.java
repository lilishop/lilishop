package cn.lili.modules.search.serviceimpl;

import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.goods.entity.dos.Category;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsSearchParamFacetRow;
import cn.lili.modules.goods.entity.dto.GoodsSkuDTO;
import cn.lili.modules.goods.entity.dto.GoodsSearchParams;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.mapper.GoodsSearchParamMapper;
import cn.lili.modules.goods.mapper.GoodsSkuMapper;
import cn.lili.modules.goods.service.CategoryService;
import cn.lili.modules.promotion.entity.dos.FullDiscount;
import cn.lili.modules.promotion.service.PromotionService;
import cn.lili.modules.search.entity.dos.GoodsRelatedInfo;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.search.entity.dos.GoodsIndex;
import cn.lili.modules.search.entity.dto.GoodsSearchDTO;
import cn.lili.modules.search.entity.dto.ParamOptions;
import cn.lili.modules.search.entity.dto.SelectorOptions;
import cn.lili.modules.search.utils.SqlFilter;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.ArgumentCaptor;
import org.springframework.test.util.ReflectionTestUtils;

import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class GoodsSearchServiceImplTest {

    /**
     * 验证商品索引构造时不自动填充上架时间。
     */
    @Test
    void goodsIndexMainConstructorLeavesReleaseTimeNullUntilExplicitlySet() {
        Assertions.assertNull(new GoodsIndex(sku("sku-1", "goods-1", "测试商品")).getReleaseTime());
        Date browseTime = new Date(1234);
        Assertions.assertEquals(browseTime, new GoodsIndex(sku("sku-1", "goods-1", "测试商品"), browseTime).getReleaseTime());
    }

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

    /**
     * 验证无搜索索引时根据MySQL促销绑定重建促销映射。
     */
    @Test
    void noEsIndexFindByIdRebuildsPromotionMapFromMysqlPromotionBinding() {
        GoodsSkuService goodsSkuService = mock(GoodsSkuService.class);
        PromotionService promotionService = mock(PromotionService.class);
        when(goodsSkuService.getGoodsSkuByIdFromCache("sku-1")).thenReturn(sku("sku-1", "goods-1", "测试商品"));
        when(promotionService.getGoodsSkuPromotionMap("store-1", "sku-1"))
                .thenReturn(fullDiscountPromotionMap("promo-1"));

        GoodsIndexServiceImpl service = new GoodsIndexServiceImpl();
        ReflectionTestUtils.setField(service, "goodsSkuService", goodsSkuService);
        ReflectionTestUtils.setField(service, "promotionService", promotionService);

        GoodsIndex result = service.findById("sku-1");

        Assertions.assertNotNull(result);
        Assertions.assertTrue(result.getPromotionMap().containsKey("FULL_DISCOUNT-promo-1"));
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
     * 验证筛选聚合查询复用公共条件并排除已选参数名称。
     */
    @Test
    void getSelectorPushdownQueriesShareBuildSharedWhereAndHideSelectedParamNames() {
        GoodsSkuService goodsSkuService = mock(GoodsSkuService.class);
        GoodsSkuMapper goodsSkuMapper = mock(GoodsSkuMapper.class);
        GoodsSearchParamMapper goodsSearchParamMapper = mock(GoodsSearchParamMapper.class);
        CategoryService categoryService = mock(CategoryService.class);
        when(goodsSkuMapper.selectSelectorBrands(any())).thenReturn(Collections.emptyList());
        when(goodsSkuMapper.selectSelectorCategoryPaths(any())).thenReturn(Collections.emptyList());
        when(goodsSearchParamMapper.selectSelectorParams(any(), any())).thenReturn(Collections.emptyList());

        GoodsSearchServiceImpl service = pushdownSelectorService(
                goodsSkuService, goodsSkuMapper, goodsSearchParamMapper, categoryService);
        GoodsSearchDTO searchDTO = new GoodsSearchDTO();
        searchDTO.setProp("内存_8G@颜色_黑色");
        searchDTO.setBrandId("brand-1");

        service.getSelector(searchDTO, new PageVO());

        ArgumentCaptor<QueryWrapper<GoodsSkuDTO>> brandCaptor = ArgumentCaptor.forClass(QueryWrapper.class);
        ArgumentCaptor<QueryWrapper<GoodsSkuDTO>> categoryCaptor = ArgumentCaptor.forClass(QueryWrapper.class);
        ArgumentCaptor<QueryWrapper<GoodsSkuDTO>> paramCaptor = ArgumentCaptor.forClass(QueryWrapper.class);
        ArgumentCaptor<List<String>> excludeCaptor = ArgumentCaptor.forClass(List.class);
        verify(goodsSkuMapper).selectSelectorBrands(brandCaptor.capture());
        verify(goodsSkuMapper).selectSelectorCategoryPaths(categoryCaptor.capture());
        verify(goodsSearchParamMapper).selectSelectorParams(paramCaptor.capture(), excludeCaptor.capture());
        verify(goodsSkuService, never()).getGoodsSkuDTOByPage(any(), any());

        QueryWrapper<GoodsSkuDTO> expected = buildSharedWhere(service, searchDTO);
        String expectedSql = expected.getCustomSqlSegment();
        Assertions.assertFalse(expectedSql.contains("ORDER BY"));
        Assertions.assertEquals(expectedSql, categoryCaptor.getValue().getCustomSqlSegment());
        Assertions.assertEquals(expectedSql, paramCaptor.getValue().getCustomSqlSegment());
        Assertions.assertTrue(expectedSql.contains("li_goods_search_param"));
        Assertions.assertEquals(2, countOccurrences(expectedSql, "li_goods_search_param"));
        Assertions.assertFalse(paramCaptor.getValue().getCustomSqlSegment().contains("NOT IN"));
        Assertions.assertEquals(List.of("内存", "颜色"), excludeCaptor.getValue());

        String brandSql = brandCaptor.getValue().getCustomSqlSegment();
        Assertions.assertTrue(brandSql.contains("gs.brand_id"));
        Assertions.assertTrue(brandSql.contains("li_goods_search_param"));
        Assertions.assertFalse(brandSql.contains("ORDER BY"));
    }

    /**
     * 验证未选择参数时聚合查询不生成NOT IN条件。
     */
    @Test
    void getSelectorPushdownOmitsNotInWhenNoParamSelected() {
        GoodsSkuService goodsSkuService = mock(GoodsSkuService.class);
        GoodsSkuMapper goodsSkuMapper = mock(GoodsSkuMapper.class);
        GoodsSearchParamMapper goodsSearchParamMapper = mock(GoodsSearchParamMapper.class);
        when(goodsSkuMapper.selectSelectorBrands(any())).thenReturn(Collections.emptyList());
        when(goodsSkuMapper.selectSelectorCategoryPaths(any())).thenReturn(Collections.emptyList());
        when(goodsSearchParamMapper.selectSelectorParams(any(), any())).thenReturn(Collections.emptyList());

        GoodsSearchServiceImpl service = pushdownSelectorService(
                goodsSkuService, goodsSkuMapper, goodsSearchParamMapper, mock(CategoryService.class));
        service.getSelector(new GoodsSearchDTO(), new PageVO());

        ArgumentCaptor<List<String>> excludeCaptor = ArgumentCaptor.forClass(List.class);
        verify(goodsSearchParamMapper).selectSelectorParams(any(), excludeCaptor.capture());
        Assertions.assertTrue(excludeCaptor.getValue() == null || excludeCaptor.getValue().isEmpty());
        Assertions.assertTrue(selectorParamSql().contains("<if test=\"excludeNames != null and excludeNames.size() > 0\">"));
        Assertions.assertTrue(selectorParamSql().contains("NOT IN"));
        Assertions.assertTrue(selectorParamSql().contains("MIN(p.param_sort) IS NULL"));
    }

    /**
     * 验证筛选聚合过程只解析一次已选参数。
     */
    @Test
    void getSelectorPushdownParsesSelectedPropsOnce() {
        GoodsSkuService goodsSkuService = mock(GoodsSkuService.class);
        GoodsSkuMapper goodsSkuMapper = mock(GoodsSkuMapper.class);
        GoodsSearchParamMapper goodsSearchParamMapper = mock(GoodsSearchParamMapper.class);
        when(goodsSkuMapper.selectSelectorBrands(any())).thenReturn(Collections.emptyList());
        when(goodsSkuMapper.selectSelectorCategoryPaths(any())).thenReturn(Collections.emptyList());
        when(goodsSearchParamMapper.selectSelectorParams(any(), any())).thenReturn(Collections.emptyList());

        GoodsSearchServiceImpl service = spy(pushdownSelectorService(
                goodsSkuService, goodsSkuMapper, goodsSearchParamMapper, mock(CategoryService.class)));
        GoodsSearchDTO searchDTO = new GoodsSearchDTO();
        searchDTO.setProp("内存_8G");
        service.getSelector(searchDTO, new PageVO());

        verify(service, times(1)).parseSelectedProps(any());
    }

    /**
     * 验证筛选参数维度中空排序排在末尾。
     */
    @Test
    void getSelectorPushdownPutsNullParamSortDimensionLast() {
        GoodsSkuService goodsSkuService = mock(GoodsSkuService.class);
        GoodsSkuMapper goodsSkuMapper = mock(GoodsSkuMapper.class);
        GoodsSearchParamMapper goodsSearchParamMapper = mock(GoodsSearchParamMapper.class);
        when(goodsSkuMapper.selectSelectorBrands(any())).thenReturn(Collections.emptyList());
        when(goodsSkuMapper.selectSelectorCategoryPaths(any())).thenReturn(Collections.emptyList());
        when(goodsSearchParamMapper.selectSelectorParams(any(), any())).thenReturn(List.of(
                facetRow("颜色", "黑色", null),
                facetRow("内存", "16G", 2),
                facetRow("内存", "8G", 2)
        ));

        GoodsSearchServiceImpl service = pushdownSelectorService(
                goodsSkuService, goodsSkuMapper, goodsSearchParamMapper, mock(CategoryService.class));
        GoodsRelatedInfo result = service.getSelector(new GoodsSearchDTO(), new PageVO());

        Assertions.assertEquals(List.of("内存", "颜色"), result.getParamOptions().stream().map(ParamOptions::getKey).toList());
        Assertions.assertEquals(List.of("16G", "8G"), result.getParamOptions().get(0).getValues());
    }

    /**
     * 验证一次查询按分类路径顺序解析类目名称。
     */
    @Test
    void getSelectorPushdownResolvesCategoryNamesInPathOrderWithSingleQuery() {
        GoodsSkuService goodsSkuService = mock(GoodsSkuService.class);
        GoodsSkuMapper goodsSkuMapper = mock(GoodsSkuMapper.class);
        GoodsSearchParamMapper goodsSearchParamMapper = mock(GoodsSearchParamMapper.class);
        CategoryService categoryService = mock(CategoryService.class);
        when(goodsSkuMapper.selectSelectorBrands(any())).thenReturn(Collections.emptyList());
        when(goodsSkuMapper.selectSelectorCategoryPaths(any())).thenReturn(List.of("c2,c1", "c3"));
        when(goodsSearchParamMapper.selectSelectorParams(any(), any())).thenReturn(Collections.emptyList());
        Category c1 = category("c1", "食品", 1);
        Category c2 = category("c2", "巧克力", 2);
        Category c3 = category("c3", "数码", 0);
        when(categoryService.listByIdsOrderByLevel(anyList())).thenReturn(List.of(c3, c1, c2));

        GoodsSearchServiceImpl service = pushdownSelectorService(
                goodsSkuService, goodsSkuMapper, goodsSearchParamMapper, categoryService);
        GoodsRelatedInfo result = service.getSelector(new GoodsSearchDTO(), new PageVO());

        Assertions.assertEquals(2, result.getCategories().size());
        Assertions.assertEquals("c2,c1", result.getCategories().get(0).getValue());
        Assertions.assertEquals("巧克力,食品", result.getCategories().get(0).getName());
        Assertions.assertEquals("c3", result.getCategories().get(1).getValue());
        Assertions.assertEquals("数码", result.getCategories().get(1).getName());
        ArgumentCaptor<List<String>> idsCaptor = ArgumentCaptor.forClass(List.class);
        verify(categoryService, times(1)).listByIdsOrderByLevel(idsCaptor.capture());
        Assertions.assertEquals(Set.of("c1", "c2", "c3"), new HashSet<>(idsCaptor.getValue()));
    }

    /**
     * 验证分类ID均为空时跳过分类查询。
     */
    @Test
    void getSelectorPushdownSkipsCategoryLookupWhenAllIdsEmpty() {
        GoodsSkuService goodsSkuService = mock(GoodsSkuService.class);
        GoodsSkuMapper goodsSkuMapper = mock(GoodsSkuMapper.class);
        GoodsSearchParamMapper goodsSearchParamMapper = mock(GoodsSearchParamMapper.class);
        CategoryService categoryService = mock(CategoryService.class);
        when(goodsSkuMapper.selectSelectorBrands(any())).thenReturn(Collections.emptyList());
        when(goodsSkuMapper.selectSelectorCategoryPaths(any())).thenReturn(Arrays.asList("", "  ", null));
        when(goodsSearchParamMapper.selectSelectorParams(any(), any())).thenReturn(Collections.emptyList());

        GoodsSearchServiceImpl service = pushdownSelectorService(
                goodsSkuService, goodsSkuMapper, goodsSearchParamMapper, categoryService);
        GoodsRelatedInfo result = service.getSelector(new GoodsSearchDTO(), new PageVO());

        Assertions.assertTrue(result.getCategories().isEmpty());
        verify(categoryService, never()).listByIdsOrderByLevel(any());
    }

    /**
     * 验证筛选聚合正确映射品牌结果。
     */
    @Test
    void getSelectorPushdownMapsBrandRows() {
        GoodsSkuService goodsSkuService = mock(GoodsSkuService.class);
        GoodsSkuMapper goodsSkuMapper = mock(GoodsSkuMapper.class);
        GoodsSearchParamMapper goodsSearchParamMapper = mock(GoodsSearchParamMapper.class);
        GoodsSkuDTO brandRow = new GoodsSkuDTO();
        brandRow.setBrandId("brand-1");
        brandRow.setBrandName("测试品牌");
        brandRow.setBrandLogo("logo.png");
        when(goodsSkuMapper.selectSelectorBrands(any())).thenReturn(List.of(brandRow));
        when(goodsSkuMapper.selectSelectorCategoryPaths(any())).thenReturn(Collections.emptyList());
        when(goodsSearchParamMapper.selectSelectorParams(any(), any())).thenReturn(Collections.emptyList());

        GoodsSearchServiceImpl service = pushdownSelectorService(
                goodsSkuService, goodsSkuMapper, goodsSearchParamMapper, mock(CategoryService.class));
        List<SelectorOptions> brands = service.getSelector(new GoodsSearchDTO(), new PageVO()).getBrands();

        Assertions.assertEquals(1, brands.size());
        Assertions.assertEquals("brand-1", brands.get(0).getValue());
        Assertions.assertEquals("测试品牌", brands.get(0).getName());
        Assertions.assertEquals("logo.png", brands.get(0).getUrl());
        Assertions.assertTrue(brandSelectorSql().contains("DISTINCT gs.brand_id"));
        Assertions.assertTrue(brandSelectorSql().contains("${ew.customSqlSegment}"));
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
     * 验证不可变MySQL查询结果仍可按价格降序排序。
     */
    @Test
    void searchGoodsByPageSortsImmutableMysqlResultByPriceDesc() {
        GoodsSkuService goodsSkuService = mock(GoodsSkuService.class);
        when(goodsSkuService.getGoodsSkuDTOByPage(any(Page.class), any())).thenReturn(new Page<>());

        GoodsSearchServiceImpl service = pushdownService(goodsSkuService);
        PageVO pageVO = new PageVO();
        pageVO.setNotConvert(true);
        pageVO.setSort("price");
        pageVO.setOrder("desc");
        pageVO.setPageNumber(1);
        pageVO.setPageSize(10);

        service.searchGoodsByPage(new GoodsSearchDTO(), pageVO);

        String sql = captureWrapper(goodsSkuService).getCustomSqlSegment();
        Assertions.assertTrue(sql.contains("ORDER BY (gs.price IS NULL), gs.price DESC, gs.id DESC"));
        Assertions.assertEquals(1, countOccurrences(sql, "ORDER BY"));
    }

    /**
     * 验证不可变MySQL查询结果仍可按销量升序排序。
     */
    @Test
    void searchGoodsByPageSortsImmutableMysqlResultByBuyCountAsc() {
        GoodsSkuService goodsSkuService = mock(GoodsSkuService.class);
        when(goodsSkuService.getGoodsSkuDTOByPage(any(Page.class), any())).thenReturn(new Page<>());

        GoodsSearchServiceImpl service = pushdownService(goodsSkuService);
        PageVO pageVO = new PageVO();
        pageVO.setNotConvert(true);
        pageVO.setSort("buyCount");
        pageVO.setOrder("asc");
        pageVO.setPageNumber(1);
        pageVO.setPageSize(10);

        service.searchGoodsByPage(new GoodsSearchDTO(), pageVO);

        String sql = captureWrapper(goodsSkuService).getCustomSqlSegment();
        Assertions.assertTrue(sql.contains("ORDER BY IFNULL(gs.buy_count,0) + IFNULL(gs.virtual_sales,0) ASC, gs.id ASC"));
        Assertions.assertFalse(sql.contains("IFNULL(gs.buy_count,0) + IFNULL(gs.virtual_sales,0) IS NULL"));
        Assertions.assertEquals(1, countOccurrences(sql, "ORDER BY"));
    }

    /**
     * 验证SQL过滤不会误判销量及上架时间字段。
     */
    @Test
    void sqlFilterDoesNotRejectBuyCountAndReleaseTime() {
        Assertions.assertFalse(SqlFilter.hit("buyCount"));
        Assertions.assertFalse(SqlFilter.hit("releaseTime"));
    }

    /**
     * 验证驼峰及下划线排序字段规范化为相同SQL表达式。
     *
     * @param sort       排序字段
     * @param expression 查询表达式
     */
    @ParameterizedTest
    @CsvSource({
            "price, gs.price",
            "buyCount, IFNULL(gs.buy_count,0) + IFNULL(gs.virtual_sales,0)",
            "buy_count, IFNULL(gs.buy_count,0) + IFNULL(gs.virtual_sales,0)",
            "releaseTime, g.create_time",
            "release_time, g.create_time",
            "commentNum, gs.comment_num",
            "comment_num, gs.comment_num"
    })
    void applyOrderByNormalizesCamelAndSnakeToSameExpression(String sort, String expression) {
        QueryWrapper<GoodsSkuDTO> wrapper = orderedWrapper(sort, "desc", true);
        String sql = wrapper.getCustomSqlSegment();
        Assertions.assertTrue(sql.contains(expression), sql);
        Assertions.assertEquals(1, countOccurrences(sql, "ORDER BY"));
        if ("g.create_time".equals(expression)) {
            Assertions.assertFalse(sql.contains("gs.create_time"));
        }
        if (expression.contains("buy_count")) {
            Assertions.assertFalse(sql.contains(expression + " IS NULL"));
        } else {
            Assertions.assertTrue(sql.contains("(" + expression + " IS NULL)"));
        }
    }

    /**
     * 验证缺少或无法识别排序字段时回退为SKU ID降序。
     */
    @Test
    void applyOrderByFallsBackToGsIdDescWhenSortMissingOrUnknown() {
        Assertions.assertTrue(orderedWrapper(null, null, true).getCustomSqlSegment().contains("ORDER BY gs.id DESC"));
        Assertions.assertTrue(orderedWrapper("unknownField", "desc", true).getCustomSqlSegment().contains("ORDER BY gs.id DESC"));
        Assertions.assertTrue(orderedWrapper("drop table", "asc", true).getCustomSqlSegment().contains("ORDER BY gs.id DESC"));
    }

    /**
     * 验证公共筛选条件不包含排序子句。
     */
    @Test
    void buildSharedWhereDoesNotContainOrderBy() {
        GoodsSearchServiceImpl service = new GoodsSearchServiceImpl(mock(GoodsSkuService.class));
        GoodsSearchDTO searchDTO = new GoodsSearchDTO();
        searchDTO.setProp("内存_8G");
        QueryWrapper<GoodsSkuDTO> wrapper = buildSharedWhere(service, searchDTO);
        String sql = wrapper.getCustomSqlSegment();
        Assertions.assertFalse(sql.contains("ORDER BY"));
        Assertions.assertTrue(sql.contains("(g.delete_flag IS NULL OR g.delete_flag = 0)"));
    }

    /**
     * 验证公共条件包含品牌排除及未删除条件。
     */
    @Test
    void buildSharedWherePushesBrandNeAndDeleteFlag() {
        GoodsSearchServiceImpl service = new GoodsSearchServiceImpl(mock(GoodsSkuService.class));
        GoodsSearchDTO searchDTO = new GoodsSearchDTO();
        searchDTO.setBrandId("brand-1@brand-2");
        searchDTO.setNeGoodsType("VIRTUAL_GOODS");
        searchDTO.setNeSalesModel("WHOLESALE");
        QueryWrapper<GoodsSkuDTO> wrapper = buildSharedWhere(service, searchDTO);
        String sql = wrapper.getCustomSqlSegment();
        Map<String, Object> pairs = wrapper.getParamNameValuePairs();

        Assertions.assertTrue(sql.contains("gs.brand_id"));
        Assertions.assertTrue(sql.contains("IN"));
        Assertions.assertTrue(pairs.containsValue("brand-1"));
        Assertions.assertTrue(pairs.containsValue("brand-2"));
        Assertions.assertTrue(sql.contains("(gs.goods_type IS NULL OR gs.goods_type <>"));
        Assertions.assertTrue(sql.contains("(gs.sales_model IS NULL OR gs.sales_model <>"));
        Assertions.assertTrue(pairs.containsValue("VIRTUAL_GOODS"));
        Assertions.assertTrue(pairs.containsValue("WHOLESALE"));
        Assertions.assertTrue(sql.contains("(g.delete_flag IS NULL OR g.delete_flag = 0)"));
        Assertions.assertFalse(sql.contains("ORDER BY"));
    }

    /**
     * 验证同名多值使用单个EXISTS加IN，不同参数名通过AND组合。
     */
    @Test
    void buildSharedWhereSameNameValuesUseSingleExistsInAndCrossNameUseAndExists() {
        GoodsSearchServiceImpl service = new GoodsSearchServiceImpl(mock(GoodsSkuService.class));
        GoodsSearchDTO sameName = new GoodsSearchDTO();
        sameName.setProp("内存_8G@内存_16G");
        QueryWrapper<GoodsSkuDTO> sameNameWrapper = buildSharedWhere(service, sameName);
        String sameNameSql = sameNameWrapper.getCustomSqlSegment();
        Assertions.assertEquals(1, countOccurrences(sameNameSql, "li_goods_search_param"));
        Assertions.assertTrue(sameNameSql.contains("IN ("));
        Assertions.assertTrue(sameNameWrapper.getParamNameValuePairs().containsValue("内存"));
        Assertions.assertTrue(sameNameWrapper.getParamNameValuePairs().containsValue("8G"));
        Assertions.assertTrue(sameNameWrapper.getParamNameValuePairs().containsValue("16G"));
        Assertions.assertFalse(sameNameSql.contains("8G"));
        Assertions.assertFalse(sameNameSql.contains("16G"));

        GoodsSearchDTO crossName = new GoodsSearchDTO();
        crossName.setProp("内存_8G@颜色_黑色");
        QueryWrapper<GoodsSkuDTO> crossNameWrapper = buildSharedWhere(service, crossName);
        String crossNameSql = crossNameWrapper.getCustomSqlSegment();
        Assertions.assertEquals(2, countOccurrences(crossNameSql, "li_goods_search_param"));
        Assertions.assertTrue(crossNameWrapper.getParamNameValuePairs().containsValue("颜色"));
        Assertions.assertTrue(crossNameWrapper.getParamNameValuePairs().containsValue("黑色"));
    }

    /**
     * 验证多个参数值绑定时占位符不冲突。
     */
    @Test
    void buildSharedWhereBindsTwelveParamValuesWithoutPlaceholderCollision() {
        GoodsSearchServiceImpl service = new GoodsSearchServiceImpl(mock(GoodsSkuService.class));
        GoodsSearchDTO searchDTO = new GoodsSearchDTO();
        searchDTO.setProp(IntStream.rangeClosed(1, 12)
                .mapToObj(i -> "内存_v" + i)
                .collect(Collectors.joining("@")));
        QueryWrapper<GoodsSkuDTO> wrapper = buildSharedWhere(service, searchDTO);
        String sql = wrapper.getCustomSqlSegment();
        Map<String, Object> pairs = wrapper.getParamNameValuePairs();

        Assertions.assertEquals(1, countOccurrences(sql, "li_goods_search_param"));
        Assertions.assertFalse(sql.contains("{10}"));
        Assertions.assertFalse(sql.contains("{1}0"));
        Assertions.assertTrue(pairs.containsValue("内存"));
        for (int i = 1; i <= 12; i++) {
            String value = "v" + i;
            Assertions.assertTrue(pairs.containsValue(value), "missing bound value " + value);
            Assertions.assertFalse(sql.contains(value), "value inlined into SQL: " + sql);
        }
    }

    /**
     * 验证下推分页限制页大小且不重复挂载分页排序项。
     */
    @Test
    void searchGoodsByPagePushdownCapsPageSizeAndDoesNotAttachPageOrderItems() {
        GoodsSkuService goodsSkuService = mock(GoodsSkuService.class);
        when(goodsSkuService.getGoodsSkuDTOByPage(any(Page.class), any())).thenReturn(new Page<>());
        GoodsSearchServiceImpl service = pushdownService(goodsSkuService);

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

        GoodsSearchServiceImpl service = pushdownService(goodsSkuService);
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

        GoodsSearchServiceImpl service = pushdownService(goodsSkuService);
        Page<GoodsIndex> result = service.searchGoodsByPage(new GoodsSearchDTO(), new PageVO());

        Assertions.assertEquals(spuCreateTime, result.getRecords().get(0).getReleaseTime());
    }

    /**
     * 构造使用SQL下推查询的商品搜索服务。
     *
     * @param goodsSkuService 商品SKU服务
     * @return 使用SQL下推查询的商品搜索服务
     */
    private GoodsSearchServiceImpl pushdownService(GoodsSkuService goodsSkuService) {
        return new GoodsSearchServiceImpl(goodsSkuService);
    }

    /**
     * 构造注入筛选聚合依赖的商品搜索服务。
     *
     * @param goodsSkuService        商品SKU服务
     * @param goodsSkuMapper         商品SKU Mapper
     * @param goodsSearchParamMapper 商品参数Mapper
     * @param categoryService        分类服务
     * @return 注入筛选聚合依赖的商品搜索服务
     */
    private GoodsSearchServiceImpl pushdownSelectorService(GoodsSkuService goodsSkuService,
                                                           GoodsSkuMapper goodsSkuMapper,
                                                           GoodsSearchParamMapper goodsSearchParamMapper,
                                                           CategoryService categoryService) {
        GoodsSearchServiceImpl service = pushdownService(goodsSkuService);
        wireSelectorDeps(service, goodsSkuMapper, goodsSearchParamMapper, categoryService);
        return service;
    }

    /**
     * 向商品搜索服务注入筛选聚合查询依赖。
     *
     * @param service                待测服务
     * @param goodsSkuMapper         商品SKU Mapper
     * @param goodsSearchParamMapper 商品参数Mapper
     * @param categoryService        分类服务
     */
    private void wireSelectorDeps(GoodsSearchServiceImpl service,
                                  GoodsSkuMapper goodsSkuMapper,
                                  GoodsSearchParamMapper goodsSearchParamMapper,
                                  CategoryService categoryService) {
        ReflectionTestUtils.setField(service, "goodsSkuMapper", goodsSkuMapper);
        ReflectionTestUtils.setField(service, "goodsSearchParamMapper", goodsSearchParamMapper);
        ReflectionTestUtils.setField(service, "categoryService", categoryService);
    }

    /**
     * 读取参数聚合Mapper中的SQL文本。
     *
     * @return 参数聚合Mapper中的SQL文本
     */
    private String selectorParamSql() {
        try (var in = GoodsSearchParamMapper.class.getResourceAsStream("/mapper/GoodsSearchParamMapper.xml")) {
            Assertions.assertNotNull(in);
            return new String(in.readAllBytes(), StandardCharsets.UTF_8);
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }
    }

    /**
     * 读取品牌聚合Mapper注解中的SQL文本。
     *
     * @return 品牌聚合Mapper注解中的SQL文本
     */
    private String brandSelectorSql() {
        try {
            return GoodsSkuMapper.class.getMethod("selectSelectorBrands",
                            com.baomidou.mybatisplus.core.conditions.Wrapper.class)
                    .getAnnotation(org.apache.ibatis.annotations.Select.class)
                    .value()[0];
        } catch (NoSuchMethodException e) {
            throw new IllegalStateException(e);
        }
    }

    /**
     * 构造参数名称、值及排序组成的聚合结果行。
     *
     * @param name  名称
     * @param value 测试值
     * @param sort  排序字段
     * @return 参数名称、值及排序组成的聚合结果行
     */
    private GoodsSearchParamFacetRow facetRow(String name, String value, Integer sort) {
        GoodsSearchParamFacetRow row = new GoodsSearchParamFacetRow();
        row.setParamName(name);
        row.setParamValue(value);
        row.setSort(sort);
        return row;
    }

    /**
     * 构造指定ID、名称及层级的分类。
     *
     * @param id    测试数据ID
     * @param name  名称
     * @param level 分类层级
     * @return 指定ID、名称及层级的分类
     */
    private Category category(String id, String name, int level) {
        Category category = new Category();
        category.setId(id);
        category.setName(name);
        category.setLevel(level);
        return category;
    }

    /**
     * 捕获商品搜索传递给SKU服务的查询条件。
     *
     * @param goodsSkuService 商品SKU服务
     * @return 商品搜索传递给SKU服务的查询条件
     */
    private QueryWrapper<GoodsSkuDTO> captureWrapper(GoodsSkuService goodsSkuService) {
        ArgumentCaptor<QueryWrapper<GoodsSkuDTO>> captor = ArgumentCaptor.forClass(QueryWrapper.class);
        verify(goodsSkuService).getGoodsSkuDTOByPage(any(Page.class), captor.capture());
        return captor.getValue();
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
     * 构造并捕获应用指定排序条件后的商品查询包装器。
     *
     * @param sort       排序字段
     * @param order      订单测试数据
     * @param notConvert 是否跳过字段名转换
     * @return 包含指定排序条件的商品查询包装器
     */
    private QueryWrapper<GoodsSkuDTO> orderedWrapper(String sort, String order, boolean notConvert) {
        GoodsSearchServiceImpl service = new GoodsSearchServiceImpl(mock(GoodsSkuService.class));
        QueryWrapper<GoodsSkuDTO> wrapper = buildSharedWhere(service, new GoodsSearchDTO());
        PageVO pageVO = new PageVO();
        pageVO.setNotConvert(notConvert);
        pageVO.setSort(sort);
        pageVO.setOrder(order);
        ReflectionTestUtils.invokeMethod(service, "applyOrderBy", wrapper, pageVO);
        return wrapper;
    }

    /**
     * 通过反射构建商品搜索的公共查询条件。
     *
     * @param service   待测服务
     * @param searchDTO 商品搜索条件
     * @return 商品搜索公共查询条件
     */
    private QueryWrapper<GoodsSkuDTO> buildSharedWhere(GoodsSearchServiceImpl service, GoodsSearchDTO searchDTO) {
        Map<String, List<String>> selectedProps = ReflectionTestUtils.invokeMethod(service, "parseSelectedProps", searchDTO);
        return ReflectionTestUtils.invokeMethod(service, "buildSharedWhere", searchDTO, selectedProps);
    }

    /**
     * 统计指定字符串片段在文本中出现的次数。
     *
     * @param text  待检查文本
     * @param token 待统计的文本片段
     * @return 指定字符串片段在文本中出现的次数
     */
    private int countOccurrences(String text, String token) {
        int count = 0;
        int idx = 0;
        while ((idx = text.indexOf(token, idx)) >= 0) {
            count++;
            idx += token.length();
        }
        return count;
    }

    /**
     * 构造商品搜索使用的SKU实体。
     *
     * @param skuId     SKU ID
     * @param goodsId   商品ID
     * @param goodsName 商品名称
     * @return 商品搜索使用的SKU实体
     */
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

    /**
     * 构造包含商品参数的SKU搜索数据。
     *
     * @param skuId      SKU ID
     * @param goodsId    商品ID
     * @param goodsName  商品名称
     * @param paramName  商品参数名称
     * @param paramValue 商品参数值
     * @return 包含商品参数的SKU搜索数据
     */
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

    /**
     * 构造指定满额优惠活动的促销映射。
     *
     * @param promotionId 促销活动ID
     * @return 指定满额优惠活动的促销映射
     */
    private Map<String, Object> fullDiscountPromotionMap(String promotionId) {
        FullDiscount fullDiscount = new FullDiscount();
        fullDiscount.setId(promotionId);
        fullDiscount.setPromotionName("满减活动");
        fullDiscount.setStoreId("store-1");
        fullDiscount.setScopeType("PORTION_GOODS");
        fullDiscount.setScopeId("sku-1");
        fullDiscount.setStartTime(new Date(System.currentTimeMillis() - 1000));
        fullDiscount.setEndTime(new Date(System.currentTimeMillis() + 3600_000));
        fullDiscount.setFullMoney(100D);
        fullDiscount.setFullMinusFlag(true);
        fullDiscount.setFullMinus(10D);
        return Map.of(PromotionTypeEnum.FULL_DISCOUNT.name() + "-" + promotionId, fullDiscount);
    }
}
