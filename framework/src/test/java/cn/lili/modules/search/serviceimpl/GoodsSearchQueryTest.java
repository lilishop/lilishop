package cn.lili.modules.search.serviceimpl;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.goods.entity.dto.GoodsSkuDTO;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.search.entity.dto.GoodsSearchDTO;
import cn.lili.modules.search.utils.SqlFilter;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static cn.lili.modules.search.serviceimpl.GoodsSearchTestFixtures.buildSharedWhere;
import static cn.lili.modules.search.serviceimpl.GoodsSearchTestFixtures.captureWrapper;
import static cn.lili.modules.search.serviceimpl.GoodsSearchTestFixtures.countOccurrences;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * 商品搜索SQL条件及排序回归测试，验证参数绑定、占位符隔离和排序白名单；不连接真实数据库。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
class GoodsSearchQueryTest {

    /**
     * 验证分页搜索将价格降序排序下推到SQL。
     */
    @Test
    void searchGoodsByPagePushesPriceDescendingOrderToSql() {
        GoodsSkuService goodsSkuService = mock(GoodsSkuService.class);
        when(goodsSkuService.getGoodsSkuDTOByPage(any(Page.class), any())).thenReturn(new Page<>());

        GoodsSearchServiceImpl service = new GoodsSearchServiceImpl(goodsSkuService);
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
     * 验证分页搜索将销量升序排序下推到SQL。
     */
    @Test
    void searchGoodsByPagePushesBuyCountAscendingOrderToSql() {
        GoodsSkuService goodsSkuService = mock(GoodsSkuService.class);
        when(goodsSkuService.getGoodsSkuDTOByPage(any(Page.class), any())).thenReturn(new Page<>());

        GoodsSearchServiceImpl service = new GoodsSearchServiceImpl(goodsSkuService);
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
            "buyCount, 'IFNULL(gs.buy_count,0) + IFNULL(gs.virtual_sales,0)'",
            "buy_count, 'IFNULL(gs.buy_count,0) + IFNULL(gs.virtual_sales,0)'",
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
        searchDTO.setNeSalesModel("RETAIL");
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
        Assertions.assertTrue(pairs.containsValue("RETAIL"));
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
}
