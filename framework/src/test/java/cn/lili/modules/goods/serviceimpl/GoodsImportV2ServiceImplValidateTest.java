package cn.lili.modules.goods.serviceimpl;

import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.goods.entity.dos.Category;
import cn.lili.modules.goods.entity.dos.GoodsUnit;
import cn.lili.modules.goods.entity.vo.GoodsImportResultVO;
import cn.lili.modules.goods.entity.vos.CategoryBrandVO;
import cn.lili.modules.goods.entity.vos.StoreGoodsLabelVO;
import cn.lili.modules.goods.service.CategoryBrandService;
import cn.lili.modules.goods.service.CategoryService;
import cn.lili.modules.goods.service.GoodsUnitService;
import cn.lili.modules.goods.service.StoreGoodsLabelService;
import cn.lili.modules.store.entity.vos.FreightTemplateVO;
import cn.lili.modules.store.service.FreightTemplateService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.test.util.ReflectionTestUtils;

import java.math.BigDecimal;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

/**
 * 商品批量导入v2：两级校验、组内继承与序号预检（非法/空缺/复用序号转失败记录）。
 *
 * @author Bulbasaur
 * @since 2026-10-20
 */
class GoodsImportV2ServiceImplValidateTest {

    private static final String STORE_ID = "store-1";

    private GoodsImportV2ServiceImpl service;
    private CategoryService categoryService;
    private CategoryBrandService categoryBrandService;
    private FreightTemplateService freightTemplateService;
    private GoodsUnitService goodsUnitService;
    private StoreGoodsLabelService storeGoodsLabelService;

    /**
     * 初始化当前用例的模拟依赖及默认合法的下拉/归属数据。
     */
    @BeforeEach
    void setUp() {
        service = spy(new GoodsImportV2ServiceImpl());
        categoryService = mock(CategoryService.class);
        categoryBrandService = mock(CategoryBrandService.class);
        freightTemplateService = mock(FreightTemplateService.class);
        goodsUnitService = mock(GoodsUnitService.class);
        storeGoodsLabelService = mock(StoreGoodsLabelService.class);
        ReflectionTestUtils.setField(service, "categoryService", categoryService);
        ReflectionTestUtils.setField(service, "categoryBrandService", categoryBrandService);
        ReflectionTestUtils.setField(service, "freightTemplateService", freightTemplateService);
        ReflectionTestUtils.setField(service, "goodsUnitService", goodsUnitService);
        ReflectionTestUtils.setField(service, "storeGoodsLabelService", storeGoodsLabelService);

        Category top = new Category();
        top.setId("c1");
        top.setParentId("0");
        top.setLevel(0);
        Category second = new Category();
        second.setId("c2");
        second.setParentId("c1");
        second.setLevel(1);
        Category leaf = new Category();
        leaf.setId("c3");
        leaf.setParentId("c2");
        leaf.setLevel(2);
        when(categoryService.getCategoryById("c1")).thenReturn(top);
        when(categoryService.getCategoryById("c2")).thenReturn(second);
        when(categoryService.getCategoryById("c3")).thenReturn(leaf);

        CategoryBrandVO brand = new CategoryBrandVO();
        brand.setId("b1");
        brand.setName("品牌一");
        when(categoryBrandService.getCategoryBrandList("c3")).thenReturn(List.of(brand));

        FreightTemplateVO freightTemplate = new FreightTemplateVO();
        freightTemplate.setId("f1");
        freightTemplate.setName("包邮");
        when(freightTemplateService.getFreightTemplateList(STORE_ID)).thenReturn(List.of(freightTemplate));

        GoodsUnit goodsUnit = new GoodsUnit();
        goodsUnit.setId("u1");
        goodsUnit.setName("件");
        when(goodsUnitService.list()).thenReturn(List.of(goodsUnit));

        when(storeGoodsLabelService.listByStoreId(STORE_ID))
                .thenReturn(List.of(new StoreGoodsLabelVO("s1", "热销", 0, BigDecimal.ONE)));
    }

    /**
     * 验证合法的多规格组（首行完整 + 次行商品级字段留空）校验通过。
     */
    @Test
    void validMultiSpecGroupPasses() {
        withStoreUser(() -> {
            List<GoodsImportV2ServiceImpl.Row> rows = List.of(
                    new GoodsImportV2ServiceImpl.Row(firstRowCells("1", "红色"), 3),
                    new GoodsImportV2ServiceImpl.Row(continuationCells("蓝色"), 4));

            assertThat(service.validateGroup(1, rows)).isNull();
        });
    }

    /**
     * 验证销售价三位小数时整组失败且原因含行号。
     */
    @Test
    void priceWithThreeDecimalsFailsWithRowNo() {
        withStoreUser(() -> {
            String[] cells = continuationCells("蓝色");
            cells[15] = "10.123";
            List<GoodsImportV2ServiceImpl.Row> rows = List.of(
                    new GoodsImportV2ServiceImpl.Row(firstRowCells("1", "红色"), 3),
                    new GoodsImportV2ServiceImpl.Row(cells, 4));

            String reason = service.validateGroup(1, rows);

            assertThat(reason).contains("第4行").contains("两位小数");
        });
    }

    /**
     * 验证组内规格值组合重复时整组失败。
     */
    @Test
    void duplicateSpecCombinationFails() {
        withStoreUser(() -> {
            List<GoodsImportV2ServiceImpl.Row> rows = List.of(
                    new GoodsImportV2ServiceImpl.Row(firstRowCells("1", "红色"), 3),
                    new GoodsImportV2ServiceImpl.Row(continuationCells("红色"), 4));

            String reason = service.validateGroup(1, rows);

            assertThat(reason).contains("第4行").contains("规格值组合");
        });
    }

    /**
     * 验证运费模板不属于当前店铺（越权使用别家模板 ID）时整组失败。
     */
    @Test
    void foreignFreightTemplateFails() {
        withStoreUser(() -> {
            String[] cells = firstRowCells("1", "红色");
            cells[5] = "f2";
            List<GoodsImportV2ServiceImpl.Row> rows = List.of(new GoodsImportV2ServiceImpl.Row(cells, 3));

            String reason = service.validateGroup(1, rows);

            assertThat(reason).contains("运费模板").contains("不属于当前店铺");
        });
    }

    /**
     * 验证平台分类 ID 不存在时整组失败。
     */
    @Test
    void notExistCategoryFails() {
        withStoreUser(() -> {
            String[] cells = firstRowCells("1", "红色");
            cells[2] = "c9-不存在分类";
            List<GoodsImportV2ServiceImpl.Row> rows = List.of(new GoodsImportV2ServiceImpl.Row(cells, 3));

            String reason = service.validateGroup(1, rows);

            assertThat(reason).contains("平台分类不存在");
        });
    }

    /**
     * 验证商品图片为空时整组失败（避免 addGoods 取首图下标越界）。
     */
    @Test
    void blankGoodsImagesFails() {
        withStoreUser(() -> {
            String[] cells = firstRowCells("1", "红色");
            cells[7] = "";
            List<GoodsImportV2ServiceImpl.Row> rows = List.of(new GoodsImportV2ServiceImpl.Row(cells, 3));

            String reason = service.validateGroup(1, rows);

            assertThat(reason).contains("商品图片不能为空");
        });
    }

    /**
     * 验证商品名称超过 50 字时整组失败。
     */
    @Test
    void goodsNameOverFiftyCharsFails() {
        withStoreUser(() -> {
            String[] cells = firstRowCells("1", "红色");
            cells[1] = "商".repeat(51);
            List<GoodsImportV2ServiceImpl.Row> rows = List.of(new GoodsImportV2ServiceImpl.Row(cells, 3));

            String reason = service.validateGroup(1, rows);

            assertThat(reason).contains("商品名称").contains("50");
        });
    }

    /**
     * 验证次行商品级字段留空时继承首行值（组内状态断言）。
     */
    @Test
    void continuationRowInheritsProductFields() {
        withStoreUser(() -> {
            List<GoodsImportV2ServiceImpl.Row> rows = new java.util.ArrayList<>(List.of(
                    new GoodsImportV2ServiceImpl.Row(firstRowCells("1", "红色"), 3),
                    new GoodsImportV2ServiceImpl.Row(continuationCells("蓝色"), 4)));

            assertThat(service.validateGroup(1, rows)).isNull();

            GoodsImportV2ServiceImpl.Row second = rows.get(1);
            assertThat(second.goodsName).isEqualTo("测试商品");
            assertThat(second.categoryPath).isEqualTo("c3-三级分类");
            assertThat(second.storeCategory).isEqualTo("s1");
            assertThat(second.brand).isEqualTo("b1");
            assertThat(second.freightTemplate).isEqualTo("f1");
            assertThat(second.goodsUnit).isEqualTo("件");
            assertThat(second.goodsImages).isEqualTo("https://example.com/a.jpg");
            assertThat(second.spec1Name).isEqualTo("颜色");
        });
    }

    /**
     * 验证非数字商品序号不抛 NumberFormatException，整段转为失败记录。
     */
    @Test
    void nonNumericGoodsNoBecomesFailRecord() {
        List<GoodsImportV2ServiceImpl.Row> rows = List.of(
                new GoodsImportV2ServiceImpl.Row(firstRowCells("abc", "红色"), 3),
                new GoodsImportV2ServiceImpl.Row(continuationCells("蓝色"), 4),
                new GoodsImportV2ServiceImpl.Row(firstRowCells("2", "均码"), 5));

        GoodsImportV2ServiceImpl.GroupingResult result = GoodsImportV2ServiceImpl.splitValidGroups(rows);

        assertThat(result.groups).containsOnlyKeys(2);
        assertThat(result.fails).hasSize(1);
        GoodsImportResultVO.FailGoods fail = result.fails.get(0);
        assertThat(fail.getRows()).isEqualTo("3-4");
        assertThat(fail.getReason()).contains("商品序号必须为数字").contains("abc");
    }

    /**
     * 验证文件首行商品序号留空的行不被静默丢弃，转为失败记录。
     */
    @Test
    void leadingBlankGoodsNoBecomesFailRecord() {
        List<GoodsImportV2ServiceImpl.Row> rows = List.of(
                new GoodsImportV2ServiceImpl.Row(firstRowCells("", "红色"), 3),
                new GoodsImportV2ServiceImpl.Row(firstRowCells("1", "均码"), 4));

        GoodsImportV2ServiceImpl.GroupingResult result = GoodsImportV2ServiceImpl.splitValidGroups(rows);

        assertThat(result.groups).containsOnlyKeys(1);
        assertThat(result.fails).hasSize(1);
        GoodsImportResultVO.FailGoods fail = result.fails.get(0);
        assertThat(fail.getRows()).isEqualTo("3");
        assertThat(fail.getReason()).contains("商品序号不能为空");
    }

    /**
     * 验证同一商品序号非连续复用时，后出现的段转为失败记录且不并入已有组。
     */
    @Test
    void reusedGoodsNoBecomesFailRecord() {
        List<GoodsImportV2ServiceImpl.Row> rows = List.of(
                new GoodsImportV2ServiceImpl.Row(firstRowCells("1", "红色"), 3),
                new GoodsImportV2ServiceImpl.Row(firstRowCells("2", "均码"), 4),
                new GoodsImportV2ServiceImpl.Row(firstRowCells("1", "蓝色"), 5),
                new GoodsImportV2ServiceImpl.Row(continuationCells("绿色"), 6));

        GoodsImportV2ServiceImpl.GroupingResult result = GoodsImportV2ServiceImpl.splitValidGroups(rows);

        assertThat(result.groups).containsOnlyKeys(1, 2);
        assertThat(result.groups.get(1)).hasSize(1);
        assertThat(result.fails).hasSize(1);
        GoodsImportResultVO.FailGoods fail = result.fails.get(0);
        assertThat(fail.getRows()).isEqualTo("5-6");
        assertThat(fail.getReason()).contains("商品序号1").contains("已在前文使用");
    }

    /**
     * 以商家身份执行校验逻辑。
     *
     * @param assertion 校验断言语句
     */
    private void withStoreUser(Runnable assertion) {
        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(storeUser());
            assertion.run();
        }
    }

    /**
     * 构造测试店铺的商家登录用户。
     *
     * @return 商家登录用户
     */
    private AuthUser storeUser() {
        return AuthUser.builder()
                .id("clerk-1")
                .storeId(STORE_ID)
                .role(UserEnums.STORE)
                .username("merchant")
                .build();
    }

    /**
     * 构造一组合法的首行 20 列数据。
     *
     * @param goodsNo    商品序号
     * @param spec1Value 规格1值
     * @return 20 列单元格数组
     */
    private String[] firstRowCells(String goodsNo, String spec1Value) {
        return new String[]{goodsNo, "测试商品", "c3-三级分类", "s1", "b1", "f1", "件",
                "https://example.com/a.jpg", "颜色", spec1Value, "", "", "", "",
                "", "10.5", "5", "100", "0.5", "0"};
    }

    /**
     * 构造一组合法的续行 20 列数据（商品级字段留空，依赖组内继承）。
     *
     * @param spec1Value 规格1值
     * @return 20 列单元格数组
     */
    private String[] continuationCells(String spec1Value) {
        return new String[]{"", "", "", "", "", "", "", "", "", spec1Value, "", "", "", "",
                "", "10.5", "5", "100", "0.5", "0"};
    }
}
