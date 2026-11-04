package cn.lili.it.search;

import cn.lili.cache.Cache;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.goods.entity.dto.RebuildStatus;
import cn.lili.modules.goods.service.GoodsSearchParamService;
import cn.lili.modules.search.entity.dos.GoodsIndex;
import cn.lili.modules.search.entity.dos.GoodsRelatedInfo;
import cn.lili.modules.search.entity.dto.GoodsSearchDTO;
import cn.lili.modules.search.entity.dto.ParamOptions;
import cn.lili.modules.search.serviceimpl.GoodsSearchServiceImpl;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicReference;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * MySQL 8.0.30 上真实执行 mapper SQL、分页、倒排同步与重建。
 *
 * @author Mike
 * @date 2026-09-03
 */
@Tag("it")
@SpringJUnitConfig(SearchMysqlITConfig.class)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class GoodsSearchMysqlIT {

    private final GoodsSearchServiceImpl goodsSearchService;
    private final GoodsSearchParamService goodsSearchParamService;
    private final Cache<RebuildStatus> cache;

    /**
     * 注入商品搜索集成测试所需服务及缓存。
     *
     * @param goodsSearchService      商品搜索服务
     * @param goodsSearchParamService 商品参数服务
     * @param cache                   测试缓存
     */
    @Autowired
    GoodsSearchMysqlIT(GoodsSearchServiceImpl goodsSearchService,
                       GoodsSearchParamService goodsSearchParamService,
                       Cache<RebuildStatus> cache) {
        this.goodsSearchService = goodsSearchService;
        this.goodsSearchParamService = goodsSearchParamService;
        this.cache = cache;
    }

    /**
     * 清空搜索相关测试表及重建进度缓存，隔离各测试用例。
     */
    @BeforeEach
    void truncate() {
        MysqlITSupport.truncateSearchTables();
        cache.clear();
    }

    /**
     * 验证搜索SQL通过索引参数存在条件筛选商品。
     */
    @Test
    void mapperSqlFiltersByIndexedParamExists() {
        seedVisibleGoods("101", SearchFixtures.indexedJson("内存", "8G", 1), "8");
        seedVisibleGoods("102", SearchFixtures.indexedJson("内存", "16G", 1), "16");
        goodsSearchParamService.syncByGoodsId("101", SearchFixtures.indexedJson("内存", "8G", 1));
        goodsSearchParamService.syncByGoodsId("102", SearchFixtures.indexedJson("内存", "16G", 1));

        Page<GoodsIndex> page = goodsSearchService.searchGoodsByPage(search("内存_8G"), page(1, 10, null, null));
        assertThat(page.getRecords()).extracting(GoodsIndex::getGoodsId).containsExactly("101");
    }

    /**
     * 验证分页总数与记录一致且同名参数多值按或匹配。
     */
    @Test
    void countMatchesRecordTotalAndSameNameValuesAreOr() {
        for (int i = 1; i <= 12; i++) {
            String goodsId = String.valueOf(200 + i);
            String value = i % 2 == 0 ? "8G" : "16G";
            seedVisibleGoods(goodsId, SearchFixtures.indexedJson("内存", value, 1), String.valueOf(i));
            goodsSearchParamService.syncByGoodsId(goodsId, SearchFixtures.indexedJson("内存", value, 1));
        }

        Page<GoodsIndex> page = goodsSearchService.searchGoodsByPage(
                search("内存_8G@内存_16G"), page(1, 5, null, null));
        assertThat(page.getTotal()).isEqualTo(12);
        assertThat(page.getRecords()).hasSize(5);
    }

    /**
     * 验证深分页结果无重复或遗漏。
     */
    @Test
    void deepPaginationHasNoDuplicatesOrGaps() {
        for (int i = 1; i <= 25; i++) {
            String goodsId = String.valueOf(300 + i);
            String skuId = String.valueOf(3000 + i);
            SearchFixtures.insertGoods(goodsId, "[]", false, LocalDateTime.of(2021, 6, 1, 0, 0));
            SearchFixtures.insertVisibleSku(skuId, goodsId, "0", new BigDecimal("99.00"), 1, 0, "c1");
        }

        Set<String> ids = new HashSet<>();
        List<String> ordered = new ArrayList<>();
        for (int pageNumber = 1; pageNumber <= 3; pageNumber++) {
            Page<GoodsIndex> page = goodsSearchService.searchGoodsByPage(
                    new GoodsSearchDTO(), page(pageNumber, 10, null, null));
            if (pageNumber < 3) {
                assertThat(page.getRecords()).hasSize(10);
            } else {
                assertThat(page.getRecords()).hasSize(5);
            }
            assertThat(page.getTotal()).isEqualTo(25);
            for (GoodsIndex index : page.getRecords()) {
                assertThat(ids.add(index.getId())).isTrue();
                ordered.add(index.getId());
            }
        }
        assertThat(ids).hasSize(25);
        List<String> expectedDesc = new ArrayList<>();
        for (int i = 25; i >= 1; i--) {
            expectedDesc.add(String.valueOf(3000 + i));
        }
        assertThat(ordered).containsExactlyElementsOf(expectedDesc);
    }

    /**
     * 验证价格为空的商品在升序及降序中均排在末尾。
     */
    @Test
    void nullPriceSortsLastInBothDirections() {
        SearchFixtures.insertGoods("401", "[]", false, LocalDateTime.of(2021, 1, 1, 0, 0));
        SearchFixtures.insertGoods("402", "[]", false, LocalDateTime.of(2021, 1, 1, 0, 0));
        SearchFixtures.insertGoods("403", "[]", false, LocalDateTime.of(2021, 1, 1, 0, 0));
        SearchFixtures.insertVisibleSku("4011", "401", "0", new BigDecimal("10.00"), 1, 0, "c1");
        SearchFixtures.insertVisibleSku("4022", "402", "0", null, 1, 0, "c1");
        SearchFixtures.insertVisibleSku("4033", "403", "0", new BigDecimal("20.00"), 1, 0, "c1");

        Page<GoodsIndex> desc = goodsSearchService.searchGoodsByPage(
                new GoodsSearchDTO(), page(1, 10, "price", "desc"));
        assertThat(desc.getRecords()).extracting(GoodsIndex::getId)
                .containsExactly("4033", "4011", "4022");

        Page<GoodsIndex> asc = goodsSearchService.searchGoodsByPage(
                new GoodsSearchDTO(), page(1, 10, "price", "asc"));
        assertThat(asc.getRecords()).extracting(GoodsIndex::getId)
                .containsExactly("4011", "4033", "4022");
    }

    /**
     * 验证二进制排序规则下参数匹配区分大小写。
     */
    @Test
    void utf8mb4BinParamMatchIsCaseSensitive() {
        seedVisibleGoods("501", SearchFixtures.indexedJson("Color", "Black", 1), "1");
        seedVisibleGoods("502", SearchFixtures.indexedJson("color", "black", 1), "2");
        goodsSearchParamService.syncByGoodsId("501", SearchFixtures.indexedJson("Color", "Black", 1));
        goodsSearchParamService.syncByGoodsId("502", SearchFixtures.indexedJson("color", "black", 1));

        Page<GoodsIndex> page = goodsSearchService.searchGoodsByPage(
                search("Color_Black"), page(1, 10, null, null));
        assertThat(page.getRecords()).extracting(GoodsIndex::getGoodsId).containsExactly("501");
        assertThat(page.getTotal()).isEqualTo(1);
    }

    /**
     * 验证同步后清空参数会删除倒排记录。
     */
    @Test
    void syncThenClearRemovesInvertedRows() {
        seedVisibleGoods("601", SearchFixtures.indexedJson("内存", "8G", 1), "1");
        goodsSearchParamService.syncByGoodsId("601", SearchFixtures.indexedJson("内存", "8G", 1));
        assertThat(SearchFixtures.countSearchParams("601")).isEqualTo(1);

        SearchFixtures.updateGoodsParams("601", "[]");
        goodsSearchParamService.syncByGoodsId("601", "[]");
        assertThat(SearchFixtures.countSearchParams("601")).isZero();
    }

    /**
     * 验证重复全量重建不会产生重复倒排记录。
     */
    @Test
    void rebuildAllTwiceDoesNotDuplicateRows() throws InterruptedException {
        String json = SearchFixtures.indexedJsonTwo("内存", "8G", 1, "颜色", "黑色", 2);
        seedVisibleGoods("701", json, "1");
        seedVisibleGoods("702", json, "2");

        goodsSearchParamService.submitRebuildAll();
        waitUntilIdle();
        int first = SearchFixtures.countSearchParams();
        assertThat(first).isEqualTo(4);

        goodsSearchParamService.submitRebuildAll();
        waitUntilIdle();
        assertThat(SearchFixtures.countSearchParams()).isEqualTo(first);
        assertThat(SearchFixtures.searchParamExists("701", "内存", "8G")).isTrue();
        assertThat(SearchFixtures.searchParamExists("702", "颜色", "黑色")).isTrue();
    }

    /**
     * 验证并发编辑与重建后倒排数据与当前参数JSON一致。
     */
    @Test
    void concurrentEditAndRebuildEndsWithCurrentJson() throws InterruptedException {
        String initial = SearchFixtures.indexedJson("内存", "8G", 1);
        String updated = SearchFixtures.indexedJson("内存", "16G", 1);
        seedVisibleGoods("801", initial, "1");
        goodsSearchParamService.syncByGoodsId("801", initial);

        CountDownLatch start = new CountDownLatch(1);
        AtomicReference<Throwable> editorError = new AtomicReference<>();
        AtomicReference<Throwable> rebuildError = new AtomicReference<>();
        Thread editor = new Thread(() -> {
            try {
                start.await();
                SearchFixtures.updateGoodsParams("801", updated);
                goodsSearchParamService.syncByGoodsId("801", updated);
            } catch (Throwable e) {
                editorError.set(e);
            }
        });
        Thread rebuilder = new Thread(() -> {
            try {
                start.await();
                goodsSearchParamService.rebuildByGoodsId("801");
            } catch (Throwable e) {
                rebuildError.set(e);
            }
        });
        editor.start();
        rebuilder.start();
        start.countDown();
        editor.join(10_000);
        rebuilder.join(10_000);

        assertThat(editorError.get()).isNull();
        assertThat(rebuildError.get()).isNull();
        assertThat(SearchFixtures.countSearchParams("801")).isEqualTo(1);
        assertThat(SearchFixtures.searchParamExists("801", "内存", "8G")
                || SearchFixtures.searchParamExists("801", "内存", "16G")).isTrue();
        goodsSearchParamService.rebuildByGoodsId("801");
        String params = currentParams("801");
        if (params.contains("16G")) {
            assertThat(SearchFixtures.searchParamExists("801", "内存", "16G")).isTrue();
            assertThat(SearchFixtures.searchParamExists("801", "内存", "8G")).isFalse();
        } else {
            assertThat(SearchFixtures.searchParamExists("801", "内存", "8G")).isTrue();
            assertThat(SearchFixtures.searchParamExists("801", "内存", "16G")).isFalse();
        }
    }

    /**
     * 验证商品索引使用SKU成本字段填充市场价。
     */
    @Test
    void goodsIndexExposesSkuCostAsMarketPrice() {
        SearchFixtures.insertGoods("1001", "[]", false, LocalDateTime.of(2021, 6, 1, 0, 0));
        SearchFixtures.insertVisibleSkuWithCost("10011", "1001", "0",
                new BigDecimal("99.00"), new BigDecimal("129.00"), 1, 0, "c1");

        Page<GoodsIndex> page = goodsSearchService.searchGoodsByPage(new GoodsSearchDTO(), page(1, 10, null, null));

        assertThat(page.getRecords()).hasSize(1);
        assertThat(page.getRecords().get(0).getMarketPrice()).isEqualTo(129.00);
    }

    /**
     * 验证参数筛选聚合查询排除已选维度的限制。
     */
    @Test
    void selectorParamSqlHidesSelectedDimension() {
        String json = SearchFixtures.indexedJsonTwo("内存", "8G", 1, "颜色", "黑色", 2);
        seedVisibleGoods("901", json, "1");
        goodsSearchParamService.syncByGoodsId("901", json);

        GoodsRelatedInfo related = goodsSearchService.getSelector(search("内存_8G"), null);
        assertThat(related.getParamOptions()).extracting(ParamOptions::getKey).containsExactly("颜色");
        assertThat(related.getParamOptions().get(0).getValues()).containsExactly("黑色");
    }

    /**
     * 插入带参数及可见SKU的商品测试数据。
     *
     * @param goodsId   商品ID
     * @param params    商品参数JSON
     * @param skuSuffix SKU编号后缀
     */
    private static void seedVisibleGoods(String goodsId, String params, String skuSuffix) {
        SearchFixtures.insertGoods(goodsId, params, false, LocalDateTime.of(2021, 6, 1, 0, 0));
        SearchFixtures.insertVisibleSku(goodsId + skuSuffix, goodsId, "0",
                new BigDecimal("99.00"), 1, 0, "c1,c2");
    }

    /**
     * 构造指定商品参数筛选条件的搜索请求。
     *
     * @param prop 商品参数筛选条件
     * @return 指定商品参数筛选条件的搜索请求
     */
    private static GoodsSearchDTO search(String prop) {
        GoodsSearchDTO searchDTO = new GoodsSearchDTO();
        searchDTO.setProp(prop);
        return searchDTO;
    }

    /**
     * 构造包含页码、页长及排序条件的分页参数。
     *
     * @param pageNumber 页码
     * @param pageSize   每页条数
     * @param sort       排序字段
     * @param order      订单测试数据
     * @return 包含页码、页长及排序条件的分页参数
     */
    private static PageVO page(int pageNumber, int pageSize, String sort, String order) {
        PageVO pageVo = new PageVO();
        pageVo.setPageNumber(pageNumber);
        pageVo.setPageSize(pageSize);
        pageVo.setSort(sort);
        pageVo.setOrder(order);
        pageVo.setNotConvert(true);
        return pageVo;
    }

    /**
     * 轮询等待商品参数重建任务结束并校验最终进度。
     */
    private void waitUntilIdle() throws InterruptedException {
        RebuildStatus status = null;
        for (int i = 0; i < 80; i++) {
            status = goodsSearchParamService.getRebuildStatus();
            if (status != null && !Boolean.TRUE.equals(status.getRunning())
                    && ("完成".equals(status.getMessage()) || status.getFinishedAt() != null)) {
                return;
            }
            Thread.sleep(100);
        }
        throw new AssertionError("rebuild did not finish: " + status);
    }

    /**
     * 读取数据库中商品当前保存的参数JSON。
     *
     * @param goodsId 商品ID
     * @return 数据库中商品当前保存的参数JSON
     */
    private static String currentParams(String goodsId) {
        try (var connection = MysqlITSupport.open();
             var statement = connection.prepareStatement("SELECT params FROM li_goods WHERE id = ?")) {
            statement.setLong(1, Long.parseLong(goodsId));
            try (var resultSet = statement.executeQuery()) {
                resultSet.next();
                return resultSet.getString(1);
            }
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }
    }
}
