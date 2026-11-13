package cn.lili.modules.search.serviceimpl;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.goods.entity.dos.Category;
import cn.lili.modules.goods.entity.dto.GoodsSearchParamFacetRow;
import cn.lili.modules.goods.entity.dto.GoodsSkuDTO;
import cn.lili.modules.goods.mapper.GoodsSearchParamMapper;
import cn.lili.modules.goods.mapper.GoodsSkuMapper;
import cn.lili.modules.goods.service.CategoryService;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.search.entity.dos.GoodsRelatedInfo;
import cn.lili.modules.search.entity.dto.GoodsSearchDTO;
import cn.lili.modules.search.entity.dto.ParamOptions;
import cn.lili.modules.search.entity.dto.SelectorOptions;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.test.util.ReflectionTestUtils;

import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static cn.lili.modules.search.serviceimpl.GoodsSearchTestFixtures.buildSharedWhere;
import static cn.lili.modules.search.serviceimpl.GoodsSearchTestFixtures.countOccurrences;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 商品搜索筛选聚合回归测试，验证公共条件复用、参数维度排序及品牌和分类映射；不连接真实数据库。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
class GoodsSearchSelectorTest {

    private final GoodsSkuService goodsSkuService = mock(GoodsSkuService.class);
    private final GoodsSkuMapper goodsSkuMapper = mock(GoodsSkuMapper.class);
    private final GoodsSearchParamMapper goodsSearchParamMapper = mock(GoodsSearchParamMapper.class);
    private final CategoryService categoryService = mock(CategoryService.class);
    private GoodsSearchServiceImpl service;

    /** 每个用例独立装配聚合依赖，默认聚合结果为空，由具体场景覆盖所需结果。 */
    @BeforeEach
    void setUp() {
        when(goodsSkuMapper.selectSelectorBrands(any())).thenReturn(Collections.emptyList());
        when(goodsSkuMapper.selectSelectorCategoryPaths(any())).thenReturn(Collections.emptyList());
        when(goodsSearchParamMapper.selectSelectorParams(any(), any())).thenReturn(Collections.emptyList());
        service = new GoodsSearchServiceImpl(goodsSkuService);
        ReflectionTestUtils.setField(service, "goodsSkuMapper", goodsSkuMapper);
        ReflectionTestUtils.setField(service, "goodsSearchParamMapper", goodsSearchParamMapper);
        ReflectionTestUtils.setField(service, "categoryService", categoryService);
    }

    /**
     * 验证筛选聚合查询复用公共条件并排除已选参数名称。
     */
    @Test
    void getSelectorPushdownQueriesShareBuildSharedWhereAndHideSelectedParamNames() {
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
        service = spy(service);
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
        when(goodsSearchParamMapper.selectSelectorParams(any(), any())).thenReturn(List.of(
                facetRow("颜色", "黑色", null),
                facetRow("内存", "16G", 2),
                facetRow("内存", "8G", 2)
        ));

        GoodsRelatedInfo result = service.getSelector(new GoodsSearchDTO(), new PageVO());

        Assertions.assertEquals(List.of("内存", "颜色"), result.getParamOptions().stream().map(ParamOptions::getKey).toList());
        Assertions.assertEquals(List.of("16G", "8G"), result.getParamOptions().get(0).getValues());
    }

    /**
     * 验证一次查询按分类路径顺序解析类目名称。
     */
    @Test
    void getSelectorPushdownResolvesCategoryNamesInPathOrderWithSingleQuery() {
        when(goodsSkuMapper.selectSelectorCategoryPaths(any())).thenReturn(List.of("c2,c1", "c3"));
        Category c1 = category("c1", "食品", 1);
        Category c2 = category("c2", "巧克力", 2);
        Category c3 = category("c3", "数码", 0);
        when(categoryService.listByIdsOrderByLevel(anyList())).thenReturn(List.of(c3, c1, c2));

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
        when(goodsSkuMapper.selectSelectorCategoryPaths(any())).thenReturn(Arrays.asList("", "  ", null));

        GoodsRelatedInfo result = service.getSelector(new GoodsSearchDTO(), new PageVO());

        Assertions.assertTrue(result.getCategories().isEmpty());
        verify(categoryService, never()).listByIdsOrderByLevel(any());
    }

    /**
     * 验证筛选聚合正确映射品牌结果。
     */
    @Test
    void getSelectorPushdownMapsBrandRows() {
        GoodsSkuDTO brandRow = new GoodsSkuDTO();
        brandRow.setBrandId("brand-1");
        brandRow.setBrandName("测试品牌");
        brandRow.setBrandLogo("logo.png");
        when(goodsSkuMapper.selectSelectorBrands(any())).thenReturn(List.of(brandRow));

        List<SelectorOptions> brands = service.getSelector(new GoodsSearchDTO(), new PageVO()).getBrands();

        Assertions.assertEquals(1, brands.size());
        Assertions.assertEquals("brand-1", brands.get(0).getValue());
        Assertions.assertEquals("测试品牌", brands.get(0).getName());
        Assertions.assertEquals("logo.png", brands.get(0).getUrl());
        Assertions.assertTrue(brandSelectorSql().contains("DISTINCT gs.brand_id"));
        Assertions.assertTrue(brandSelectorSql().contains("${ew.customSqlSegment}"));
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
}
