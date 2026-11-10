package cn.lili.modules.goods.serviceimpl;

import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.goods.entity.dos.Brand;
import cn.lili.modules.goods.entity.dos.GoodsUnit;
import cn.lili.modules.goods.entity.vos.CategoryVO;
import cn.lili.modules.goods.entity.vos.StoreGoodsLabelVO;
import cn.lili.modules.goods.service.BrandService;
import cn.lili.modules.goods.service.CategoryService;
import cn.lili.modules.goods.service.GoodsUnitService;
import cn.lili.modules.goods.service.StoreGoodsLabelService;
import cn.lili.modules.store.entity.dos.StoreDetail;
import cn.lili.modules.store.entity.vos.FreightTemplateVO;
import cn.lili.modules.store.service.FreightTemplateService;
import cn.lili.modules.store.service.StoreDetailService;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.test.util.ReflectionTestUtils;

import java.io.ByteArrayInputStream;
import java.math.BigDecimal;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * 商品批量导入v2：downloadTemplate 模板下载——验证响应头（xlsx Content-Type 与 URLEncoder 文件名）、
 * 说明行关键提示语、第 2 行 20 列表头与解析层 Row 字段顺序一致、隐藏 sheet 下拉数据源（id-名称 格式）
 * 与主 sheet 数据校验区域存在，以及非商家用户拒绝下载。
 *
 * @author Bulbasaur
 * @since 2026-10-20
 */
class GoodsImportV2TemplateTest {

    private static final String STORE_ID = "store-1";

    /**
     * 与解析层 20 列（A-T）一一对应的期望表头文案，硬编码以保证模板列顺序不被悄悄改动
     */
    private static final String[] EXPECTED_HEADERS = {
            "商品序号", "商品名称", "平台分类", "店铺分类", "品牌",
            "运费模板", "计量单位", "商品图片", "规格1名称", "规格1值",
            "规格2名称", "规格2值", "规格3名称", "规格3值", "SKU编码",
            "销售价", "成本价", "库存", "重量", "预警库存"};

    private GoodsImportV2ServiceImpl service;

    /**
     * 初始化模板下载所需的全部数据源 mock：店铺经营类目（三级分类树）、本店运费模板、
     * 平台计量单位、全量品牌、本店店铺分类。
     */
    @BeforeEach
    void setUp() {
        service = new GoodsImportV2ServiceImpl();
        CategoryService categoryService = mock(CategoryService.class);
        FreightTemplateService freightTemplateService = mock(FreightTemplateService.class);
        GoodsUnitService goodsUnitService = mock(GoodsUnitService.class);
        BrandService brandService = mock(BrandService.class);
        StoreGoodsLabelService storeGoodsLabelService = mock(StoreGoodsLabelService.class);
        StoreDetailService storeDetailService = mock(StoreDetailService.class);
        ReflectionTestUtils.setField(service, "categoryService", categoryService);
        ReflectionTestUtils.setField(service, "freightTemplateService", freightTemplateService);
        ReflectionTestUtils.setField(service, "goodsUnitService", goodsUnitService);
        ReflectionTestUtils.setField(service, "brandService", brandService);
        ReflectionTestUtils.setField(service, "storeGoodsLabelService", storeGoodsLabelService);
        ReflectionTestUtils.setField(service, "storeDetailService", storeDetailService);

        StoreDetail storeDetail = new StoreDetail();
        storeDetail.setGoodsManagementCategory("c1");
        when(storeDetailService.getStoreDetail(STORE_ID)).thenReturn(storeDetail);

        CategoryVO top = category("c1", "一级分类");
        CategoryVO second = category("c2", "二级分类");
        CategoryVO leaf = category("c3", "三级分类");
        // CategoryVO.getChildren 会原地排序，children 必须是可变列表
        second.setChildren(new ArrayList<>(List.of(leaf)));
        top.setChildren(new ArrayList<>(List.of(second)));
        when(categoryService.getStoreCategory(any(String[].class))).thenReturn(List.of(top));

        FreightTemplateVO freightTemplate = new FreightTemplateVO();
        freightTemplate.setId("f1");
        freightTemplate.setName("包邮");
        when(freightTemplateService.getFreightTemplateList(STORE_ID)).thenReturn(List.of(freightTemplate));

        GoodsUnit goodsUnit = new GoodsUnit();
        goodsUnit.setId("u1");
        goodsUnit.setName("件");
        when(goodsUnitService.list()).thenReturn(List.of(goodsUnit));

        Brand brand = new Brand();
        brand.setId("b1");
        brand.setName("品牌一");
        when(brandService.listNotDeleted()).thenReturn(List.of(brand));

        when(storeGoodsLabelService.listByStoreId(STORE_ID))
                .thenReturn(List.of(new StoreGoodsLabelVO("s1", "热销", 0, BigDecimal.ONE)));
    }

    /**
     * 验证模板下载：响应头为 xlsx 且文件名 URLEncoder 编码；说明行包含简报指定的四类提示；
     * 第 2 行恰好 20 列表头且文案/顺序与解析层一致；存在隐藏 sheet 承载 id-名称 下拉数据源
     * （分类/店铺分类/品牌/运费模板/计量单位全覆盖），主 sheet 挂有数据校验（下拉）。
     */
    @Test
    void templateContainsInstructionHeadersAndHiddenDropdowns() throws Exception {
        MockHttpServletResponse response = new MockHttpServletResponse();
        withStoreUser(() -> service.downloadTemplate(response));

        assertThat(response.getContentType())
                .isEqualTo("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
        assertThat(response.getHeader("Content-Disposition"))
                .contains(URLEncoder.encode("商品导入模板.xlsx", "UTF-8"));

        try (Workbook workbook = WorkbookFactory.create(new ByteArrayInputStream(response.getContentAsByteArray()))) {
            Sheet sheet = workbook.getSheetAt(0);

            String instruction = sheet.getRow(0).getCell(0).getStringCellValue();
            assertThat(instruction)
                    .contains("商品级字段只填首行")
                    .contains("名称勿含英文逗号")
                    .contains("图片须先上传素材中心")
                    .contains("一行一个SKU")
                    .contains("同商品填相同序号");

            Row header = sheet.getRow(1);
            for (int i = 0; i < EXPECTED_HEADERS.length; i++) {
                assertThat(header.getCell(i).getStringCellValue())
                        .as("第%s列表头", (char) ('A' + i))
                        .isEqualTo(EXPECTED_HEADERS[i]);
            }
            assertThat(header.getCell(EXPECTED_HEADERS.length)).isNull();

            assertThat(workbook.getNumberOfSheets()).isGreaterThan(1);
            List<String> hiddenValues = new ArrayList<>();
            for (int i = 1; i < workbook.getNumberOfSheets(); i++) {
                assertThat(workbook.isSheetHidden(i)).as("第%s个sheet应隐藏", i).isTrue();
                Sheet hiddenSheet = workbook.getSheetAt(i);
                for (int r = 0; r <= hiddenSheet.getLastRowNum(); r++) {
                    Row row = hiddenSheet.getRow(r);
                    if (row != null && row.getCell(0) != null) {
                        hiddenValues.add(row.getCell(0).getStringCellValue());
                    }
                }
            }
            assertThat(hiddenValues).contains(
                    "c3-三级分类", "s1-热销", "b1-品牌一", "f1-包邮", "u1-件");
            assertThat(sheet.getDataValidations()).isNotEmpty();
        }
    }

    /**
     * 验证非商家登录上下文（无当前用户）下载模板被拒绝，抛 ServiceException。
     */
    @Test
    void anonymousUserCannotDownloadTemplate() {
        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(null);
            assertThatThrownBy(() -> service.downloadTemplate(new MockHttpServletResponse()))
                    .isInstanceOf(ServiceException.class);
        }
    }

    /**
     * 以商家身份执行下载逻辑。
     *
     * @param action 下载调用
     */
    private void withStoreUser(Runnable action) {
        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(AuthUser.builder()
                    .id("clerk-1")
                    .storeId(STORE_ID)
                    .role(UserEnums.STORE)
                    .username("merchant")
                    .build());
            action.run();
        }
    }

    /**
     * 构造分类树节点（sortOrder 必填：CategoryVO.getChildren 按其排序，空值会 NPE）。
     *
     * @param id   分类 ID
     * @param name 分类名称
     * @return 分类 VO
     */
    private static CategoryVO category(String id, String name) {
        CategoryVO category = new CategoryVO();
        category.setId(id);
        category.setName(name);
        category.setSortOrder(BigDecimal.ONE);
        return category;
    }
}
