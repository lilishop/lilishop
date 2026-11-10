package cn.lili.modules.goods.serviceimpl;

import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.goods.entity.dos.Category;
import cn.lili.modules.goods.entity.dos.GoodsUnit;
import cn.lili.modules.goods.entity.dto.GoodsOperationDTO;
import cn.lili.modules.goods.entity.vo.GoodsImportResultVO;
import cn.lili.modules.goods.entity.vos.CategoryBrandVO;
import cn.lili.modules.goods.entity.vos.StoreGoodsLabelVO;
import cn.lili.modules.goods.service.CategoryBrandService;
import cn.lili.modules.goods.service.CategoryService;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.modules.goods.service.GoodsUnitService;
import cn.lili.modules.goods.service.StoreGoodsLabelService;
import cn.lili.modules.store.entity.vos.FreightTemplateVO;
import cn.lili.modules.store.service.FreightTemplateService;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.util.ReflectionTestUtils;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * 商品批量导入v2：importGoods 主流程（部分成功、DTO 组装规则、文件级上限与后缀校验）。
 *
 * @author Bulbasaur
 * @since 2026-10-20
 */
class GoodsImportV2ServiceImplImportTest {

    private static final String STORE_ID = "store-1";

    private GoodsImportV2ServiceImpl service;
    private GoodsService goodsService;

    /**
     * 初始化当前用例的模拟依赖及默认合法的下拉/归属数据（与 GoodsImportV2ServiceImplValidateTest 同一套）。
     */
    @BeforeEach
    void setUp() {
        service = new GoodsImportV2ServiceImpl();
        goodsService = mock(GoodsService.class);
        CategoryService categoryService = mock(CategoryService.class);
        CategoryBrandService categoryBrandService = mock(CategoryBrandService.class);
        FreightTemplateService freightTemplateService = mock(FreightTemplateService.class);
        GoodsUnitService goodsUnitService = mock(GoodsUnitService.class);
        StoreGoodsLabelService storeGoodsLabelService = mock(StoreGoodsLabelService.class);
        ReflectionTestUtils.setField(service, "goodsService", goodsService);
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
     * 验证部分成功：组1 合法、组2 销售价非法 → successCount=1、failCount=1，
     * addGoods 仅对合法组调用一次，且组装出的 DTO 符合约定（release=false、goodsType=PHYSICAL_GOODS、
     * price 取组内最低价、skuList 不含 sn 键、规格名→规格值动态键、预警库存留空按 0 写入）。
     */
    @Test
    void partialSuccessAssemblesGoodsOperationDTO() throws Exception {
        String[] badPriceCells = firstRowCells("2", "均码");
        badPriceCells[15] = "abc";
        MockMultipartFile file = xlsxFile(List.<String[]>of(
                firstRowCells("1", "红色"),
                continuationCells("蓝色", "8.5"),
                badPriceCells));
        when(goodsService.addGoods(any())).thenReturn("goods-1");

        GoodsImportResultVO result = withStoreUser(() -> service.importGoods(file));

        assertThat(result.getSuccessCount()).isEqualTo(1);
        assertThat(result.getFailCount()).isEqualTo(1);
        assertThat(result.getFailList()).hasSize(1);
        GoodsImportResultVO.FailGoods fail = result.getFailList().get(0);
        assertThat(fail.getGoodsNo()).isEqualTo(2);
        assertThat(fail.getRows()).isEqualTo("5");
        assertThat(fail.getReason()).contains("第5行").contains("格式不正确");

        ArgumentCaptor<GoodsOperationDTO> captor = ArgumentCaptor.forClass(GoodsOperationDTO.class);
        verify(goodsService, times(1)).addGoods(captor.capture());
        GoodsOperationDTO dto = captor.getValue();
        assertThat(dto.getRelease()).isFalse();
        assertThat(dto.getGoodsType()).isEqualTo("PHYSICAL_GOODS");
        assertThat(dto.getGoodsName()).isEqualTo("测试商品");
        assertThat(dto.getPrice()).isEqualTo(8.5);
        assertThat(dto.getCategoryPath()).isEqualTo("c1,c2,c3");
        assertThat(dto.getStoreCategoryPath()).isEqualTo("s1");
        assertThat(dto.getBrandId()).isEqualTo("b1");
        assertThat(dto.getTemplateId()).isEqualTo("f1");
        assertThat(dto.getGoodsUnit()).isEqualTo("件");
        assertThat(dto.getIntro()).isEmpty();
        assertThat(dto.getMobileIntro()).isEmpty();
        assertThat(dto.getGoodsParamsDTOList()).isNull();
        assertThat(dto.getGoodsGalleryList()).containsExactly("https://example.com/a.jpg");

        assertThat(dto.getSkuList()).hasSize(2);
        Map<String, Object> sku1 = dto.getSkuList().get(0);
        assertThat(sku1).doesNotContainKey("sn");
        assertThat(sku1.get("颜色")).isEqualTo("红色");
        assertThat(sku1.get("price")).isEqualTo(10.5);
        assertThat(sku1.get("cost")).isEqualTo(5.0);
        assertThat(sku1.get("quantity")).isEqualTo(100);
        assertThat(sku1.get("weight")).isEqualTo(0.5);
        assertThat(sku1.get("alertQuantity")).isEqualTo(0);
        assertThat(sku1.get("images")).isEqualTo(List.of("https://example.com/a.jpg"));
        Map<String, Object> sku2 = dto.getSkuList().get(1);
        assertThat(sku2).doesNotContainKey("sn");
        assertThat(sku2.get("颜色")).isEqualTo("蓝色");
        assertThat(sku2.get("price")).isEqualTo(8.5);
    }

    /**
     * 验证 addGoods 抛 ServiceException 时该组记入 failList（原因为异常消息），后续组照常导入。
     */
    @Test
    void addGoodsFailureDoesNotInterruptFollowingGroups() throws Exception {
        MockMultipartFile file = xlsxFile(List.<String[]>of(
                firstRowCells("1", "红色"),
                firstRowCells("2", "均码")));
        when(goodsService.addGoods(any()))
                .thenThrow(new ServiceException("商品名称包含敏感词"))
                .thenReturn("goods-2");

        GoodsImportResultVO result = withStoreUser(() -> service.importGoods(file));

        assertThat(result.getSuccessCount()).isEqualTo(1);
        assertThat(result.getFailCount()).isEqualTo(1);
        GoodsImportResultVO.FailGoods fail = result.getFailList().get(0);
        assertThat(fail.getGoodsNo()).isEqualTo(1);
        assertThat(fail.getRows()).isEqualTo("3");
        assertThat(fail.getReason()).contains("敏感词");
        verify(goodsService, times(2)).addGoods(any());
    }

    /**
     * 验证数据行超过 2000 行时整个文件拒绝，抛 ServiceException 且不调用 addGoods。
     */
    @Test
    void overTwoThousandRowsRejected() throws Exception {
        List<String[]> dataRows = new ArrayList<>();
        dataRows.add(firstRowCells("1", "红色"));
        for (int i = 0; i < 2000; i++) {
            dataRows.add(continuationCells("蓝色", "10"));
        }
        MockMultipartFile file = xlsxFile(dataRows);

        assertThatThrownBy(() -> service.importGoods(file))
                .isInstanceOf(ServiceException.class)
                .hasMessageContaining("2000");
        verifyNoInteractions(goodsService);
    }

    /**
     * 验证商品序号数超过 500 个时整个文件拒绝，抛 ServiceException 且不调用 addGoods。
     */
    @Test
    void overFiveHundredGoodsRejected() throws Exception {
        List<String[]> dataRows = new ArrayList<>();
        for (int i = 1; i <= 501; i++) {
            dataRows.add(firstRowCells(String.valueOf(i), "均码"));
        }
        MockMultipartFile file = xlsxFile(dataRows);

        assertThatThrownBy(() -> service.importGoods(file))
                .isInstanceOf(ServiceException.class)
                .hasMessageContaining("500");
        verifyNoInteractions(goodsService);
    }

    /**
     * 验证无规格商品（规格1名称留空）：skuList 仅一条，map 内无规格动态键，只有价格/库存/图片等固定键。
     */
    @Test
    void singleSpecGroupHasNoDynamicSpecKeys() throws Exception {
        String[] cells = firstRowCells("1", "");
        cells[8] = "";
        MockMultipartFile file = xlsxFile(List.<String[]>of(cells));
        when(goodsService.addGoods(any())).thenReturn("goods-1");

        GoodsImportResultVO result = withStoreUser(() -> service.importGoods(file));

        assertThat(result.getSuccessCount()).isEqualTo(1);
        assertThat(result.getFailCount()).isEqualTo(0);
        ArgumentCaptor<GoodsOperationDTO> captor = ArgumentCaptor.forClass(GoodsOperationDTO.class);
        verify(goodsService, times(1)).addGoods(captor.capture());
        Map<String, Object> sku = captor.getValue().getSkuList().get(0);
        assertThat(captor.getValue().getSkuList()).hasSize(1);
        assertThat(sku).doesNotContainKey("sn");
        assertThat(sku.keySet()).containsExactlyInAnyOrder(
                "price", "cost", "quantity", "weight", "alertQuantity", "images");
    }

    /**
     * 验证非 .xlsx 后缀的文件被拒绝。
     */
    @Test
    void nonXlsxFileRejected() {
        MockMultipartFile file = new MockMultipartFile("files", "goods.xls", null, new byte[0]);

        assertThatThrownBy(() -> service.importGoods(file))
                .isInstanceOf(ServiceException.class)
                .hasMessageContaining("xlsx");
        verifyNoInteractions(goodsService);
    }

    /**
     * 验证内容损坏的 xlsx（解析期抛 RuntimeException）被包装为友好的 ServiceException，
     * 提示商家使用下载的模板，而不是暴露 POI 内部异常。
     */
    @Test
    void corruptXlsxContentWrappedAsFriendlyServiceException() {
        MockMultipartFile file = new MockMultipartFile("files", "goods.xlsx", null, new byte[0]) {
            @Override
            public InputStream getInputStream() {
                return new InputStream() {
                    private final byte[] head = {'P', 'K', 3, 4};
                    private int pos = 0;

                    @Override
                    public int read() {
                        if (pos < head.length) {
                            return head[pos++] & 0xff;
                        }
                        throw new IllegalStateException("模拟损坏的xlsx内容");
                    }
                };
            }
        };

        assertThatThrownBy(() -> service.importGoods(file))
                .isInstanceOf(ServiceException.class)
                .hasMessageContaining("模板");
        verifyNoInteractions(goodsService);
    }

    /**
     * 以商家身份执行导入逻辑。
     *
     * @param action 导入调用
     * @return 导入结果
     */
    private GoodsImportResultVO withStoreUser(java.util.function.Supplier<GoodsImportResultVO> action) {
        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(storeUser());
            return action.get();
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
     * 将 20 列数据行数组写成内存 xlsx 并包装为 MultipartFile：第 1 行说明、第 2 行表头、第 3 行起数据。
     *
     * @param dataRows 数据行，每行 20 列字符串，空串表示留空
     * @return xlsx 导入文件
     */
    private MockMultipartFile xlsxFile(List<String[]> dataRows) throws Exception {
        try (Workbook workbook = new XSSFWorkbook(); ByteArrayOutputStream out = new ByteArrayOutputStream()) {
            Sheet sheet = workbook.createSheet("商品导入");
            sheet.createRow(0).createCell(0).setCellValue("填写说明");
            sheet.createRow(1).createCell(0).setCellValue("表头");
            for (int i = 0; i < dataRows.size(); i++) {
                org.apache.poi.ss.usermodel.Row row = sheet.createRow(i + 2);
                String[] cells = dataRows.get(i);
                for (int c = 0; c < cells.length; c++) {
                    if (!cells[c].isEmpty()) {
                        row.createCell(c).setCellValue(cells[c]);
                    }
                }
            }
            workbook.write(out);
            return new MockMultipartFile("files", "goods.xlsx", null, out.toByteArray());
        }
    }

    /**
     * 构造一组合法的首行 20 列数据（预警库存留空，验证组装层按 0 写入）。
     *
     * @param goodsNo    商品序号
     * @param spec1Value 规格1值
     * @return 20 列单元格数组
     */
    private String[] firstRowCells(String goodsNo, String spec1Value) {
        return new String[]{goodsNo, "测试商品", "c3-三级分类", "s1", "b1", "f1", "件",
                "https://example.com/a.jpg", "颜色", spec1Value, "", "", "", "",
                "", "10.5", "5", "100", "0.5", ""};
    }

    /**
     * 构造一组合法的续行 20 列数据（商品级字段留空，依赖组内继承）。
     *
     * @param spec1Value 规格1值
     * @param price      销售价
     * @return 20 列单元格数组
     */
    private String[] continuationCells(String spec1Value, String price) {
        return new String[]{"", "", "", "", "", "", "", "", "", spec1Value, "", "", "", "",
                "", price, "5", "100", "0.5", ""};
    }
}
