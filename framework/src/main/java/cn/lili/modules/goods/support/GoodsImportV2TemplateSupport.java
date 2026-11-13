package cn.lili.modules.goods.support;

import cn.lili.common.exception.ServiceException;
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
import org.apache.poi.ss.usermodel.DataValidationConstraint;
import org.apache.poi.ss.usermodel.Name;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.ss.util.CellRangeAddressList;
import org.apache.poi.xssf.usermodel.XSSFDataValidation;
import org.apache.poi.xssf.usermodel.XSSFDataValidationHelper;
import org.apache.poi.xssf.usermodel.XSSFRow;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;

import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

/**
 * 商品批量导入v2 Excel 模板生成支持组件
 * 负责组装平台分类、店铺分类、品牌、运费模板、计量单位等下拉选项与数据验证规则，生成带有说明行与隐藏数据源 sheet 的 .xlsx 模板文件。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
public class GoodsImportV2TemplateSupport {

    /**
     * 模板列数（A-T 共 20 列）
     */
    public static final int COLUMN_COUNT = 20;

    /**
     * 模板第 2 行的 20 列表头文案，顺序与数据行字段（A-T 列）一一对应，列错位会导致整列解析错位
     */
    public static final String[] TEMPLATE_HEADERS = {
            "商品序号", "商品名称", "平台分类", "店铺分类", "品牌",
            "运费模板", "计量单位", "商品图片", "规格1名称", "规格1值",
            "规格2名称", "规格2值", "规格3名称", "规格3值", "SKU编码",
            "销售价", "成本价", "库存", "重量", "预警库存"};

    /**
     * 模板第 1 行（合并 A1:T1）的填写说明：分组规则、商品级字段只填首行、名称逗号与图片素材限制
     */
    public static final String TEMPLATE_INSTRUCTION =
            "填写说明：1.一行一个SKU、同商品填相同序号（续行商品序号留空自动归属上一商品）；"
                    + "2.商品级字段只填首行（商品名称、平台分类、店铺分类、品牌、运费模板、计量单位、商品图片、规格名称），续行留空；"
                    + "3.商品名称勿含英文逗号；"
                    + "4.商品图片须先上传素材中心，再复制图片URL填写，多个用英文逗号分隔、首张为主图；"
                    + "5.平台分类、店铺分类、品牌、运费模板、计量单位可从下拉选择（id-名称格式）。";

    /**
     * 数据起始行下标（从 0 开始，跳过第 1 行说明与第 2 行表头）
     */
    public static final int DATA_START_ROW_INDEX = 2;

    /**
     * 单次导入数据行上限（不含说明行与表头），超过即整文件拒绝
     */
    public static final int MAX_DATA_ROWS = 2000;

    /**
     * 单次导入商品数量上限（按商品序号分组计数），超过即整文件拒绝
     */
    public static final int MAX_GOODS_COUNT = 500;

    /**
     * 生成并写出商品批量导入模板 Excel
     *
     * @param response               HTTP 响应对象
     * @param storeId                当前店铺 ID
     * @param categoryService        分类业务服务
     * @param storeGoodsLabelService 店铺分类业务服务
     * @param brandService           品牌业务服务
     * @param freightTemplateService 运费模板业务服务
     * @param goodsUnitService       计量单位业务服务
     * @param storeDetailService     店铺详情业务服务
     */
    public static void downloadTemplate(HttpServletResponse response,
                                        String storeId,
                                        CategoryService categoryService,
                                        StoreGoodsLabelService storeGoodsLabelService,
                                        BrandService brandService,
                                        FreightTemplateService freightTemplateService,
                                        GoodsUnitService goodsUnitService,
                                        StoreDetailService storeDetailService) {
        if (storeId == null) {
            throw new ServiceException("当前登录用户不是商家，无法下载商品导入模板");
        }
        try (XSSFWorkbook workbook = new XSSFWorkbook()) {
            XSSFSheet sheet = workbook.createSheet("导入模板");
            XSSFRow instructionRow = sheet.createRow(0);
            instructionRow.createCell(0).setCellValue(TEMPLATE_INSTRUCTION);
            sheet.addMergedRegion(new CellRangeAddress(0, 0, 0, COLUMN_COUNT - 1));
            XSSFRow headerRow = sheet.createRow(1);
            for (int i = 0; i < TEMPLATE_HEADERS.length; i++) {
                headerRow.createCell(i).setCellValue(TEMPLATE_HEADERS[i]);
                sheet.setColumnWidth(i, 4000);
            }
            sheet.setColumnWidth(1, 8000);
            sheet.setColumnWidth(7, 8000);

            addDropdown(workbook, sheet, "hiddenCategory",
                    categoryOptions(storeId, storeDetailService, categoryService), 2);
            addDropdown(workbook, sheet, "hiddenStoreCategory",
                    storeCategoryOptions(storeId, storeGoodsLabelService), 3);
            addDropdown(workbook, sheet, "hiddenBrand",
                    brandOptions(brandService), 4);
            addDropdown(workbook, sheet, "hiddenFreightTemplate",
                    freightTemplateOptions(storeId, freightTemplateService), 5);
            addDropdown(workbook, sheet, "hiddenGoodsUnit",
                    goodsUnitOptions(goodsUnitService), 6);

            response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
            response.setHeader("Content-Disposition",
                    "attachment;filename=" + URLEncoder.encode("商品导入模板.xlsx", StandardCharsets.UTF_8.name()));
            workbook.write(response.getOutputStream());
        } catch (IOException e) {
            throw new ServiceException("商品导入模板生成失败：" + e.getMessage());
        }
    }

    /**
     * 收集平台分类下拉数据源：按店铺经营类目取分类树，遍历到第三级叶子，拼为「分类ID-名称」。
     *
     * @param storeId            当前店铺 ID
     * @param storeDetailService 店铺详情服务
     * @param categoryService    分类服务
     * @return 叶子三级分类的 id-名称 列表；店铺经营类目未配置时为空列表
     */
    private static List<String> categoryOptions(String storeId,
                                                StoreDetailService storeDetailService,
                                                CategoryService categoryService) {
        List<String> options = new ArrayList<>();
        StoreDetail storeDetail = storeDetailService.getStoreDetail(storeId);
        if (storeDetail == null || storeDetail.getGoodsManagementCategory() == null
                || storeDetail.getGoodsManagementCategory().isEmpty()) {
            return options;
        }
        List<CategoryVO> topCategories =
                categoryService.getStoreCategory(storeDetail.getGoodsManagementCategory().split(","));
        if (topCategories == null) {
            return options;
        }
        for (CategoryVO top : topCategories) {
            if (top.getChildren() == null) {
                continue;
            }
            for (CategoryVO second : top.getChildren()) {
                if (second.getChildren() == null) {
                    continue;
                }
                for (CategoryVO leaf : second.getChildren()) {
                    options.add(leaf.getId() + "-" + leaf.getName());
                }
            }
        }
        return options;
    }

    /**
     * 收集店铺分类下拉数据源（本店店铺分类，「ID-名称」）。
     *
     * @param storeId                当前店铺 ID
     * @param storeGoodsLabelService 店铺分类服务
     * @return 店铺分类的 id-名称 列表
     */
    private static List<String> storeCategoryOptions(String storeId, StoreGoodsLabelService storeGoodsLabelService) {
        List<String> options = new ArrayList<>();
        for (StoreGoodsLabelVO label : storeGoodsLabelService.listByStoreId(storeId)) {
            options.add(label.getId() + "-" + label.getLabelName());
        }
        return options;
    }

    /**
     * 收集品牌下拉数据源：静态模板无法按所选分类联动，放全量品牌「ID-名称」供选择
     *
     * @param brandService 品牌服务
     * @return 全量品牌的 id-名称 列表
     */
    private static List<String> brandOptions(BrandService brandService) {
        List<String> options = new ArrayList<>();
        for (Brand brand : brandService.listNotDeleted()) {
            options.add(brand.getId() + "-" + brand.getName());
        }
        return options;
    }

    /**
     * 收集运费模板下拉数据源（本店运费模板，「ID-名称」）。
     *
     * @param storeId                当前店铺 ID
     * @param freightTemplateService 运费模板服务
     * @return 运费模板的 id-名称 列表
     */
    private static List<String> freightTemplateOptions(String storeId, FreightTemplateService freightTemplateService) {
        List<String> options = new ArrayList<>();
        for (FreightTemplateVO template : freightTemplateService.getFreightTemplateList(storeId)) {
            options.add(template.getId() + "-" + template.getName());
        }
        return options;
    }

    /**
     * 收集计量单位下拉数据源（平台计量单位，「ID-名称」）。
     *
     * @param goodsUnitService 计量单位服务
     * @return 计量单位的 id-名称 列表
     */
    private static List<String> goodsUnitOptions(GoodsUnitService goodsUnitService) {
        List<String> options = new ArrayList<>();
        for (GoodsUnit goodsUnit : goodsUnitService.list()) {
            options.add(goodsUnit.getId() + "-" + goodsUnit.getName());
        }
        return options;
    }

    /**
     * 为模板主 sheet 的某一列添加下拉：候选项写入隐藏 sheet 的 A 列，通过命名区域引用，再绑定到数据行区域。
     *
     * @param workbook        工作簿
     * @param mainSheet       模板主 sheet
     * @param hiddenSheetName 隐藏 sheet 名（同时作为命名区域名，仅用 ASCII 避免公式转义）
     * @param options         下拉候选项（id-名称）
     * @param columnIndex     主 sheet 目标列下标（从 0 开始）
     */
    private static void addDropdown(XSSFWorkbook workbook, XSSFSheet mainSheet, String hiddenSheetName,
                                    List<String> options, int columnIndex) {
        XSSFSheet hidden = workbook.createSheet(hiddenSheetName);
        for (int i = 0; i < options.size(); i++) {
            hidden.createRow(i).createCell(0).setCellValue(options.get(i));
        }
        Name name = workbook.createName();
        name.setNameName(hiddenSheetName);
        name.setRefersToFormula(hiddenSheetName + "!$A$1:$A$" + Math.max(options.size(), 1));
        workbook.setSheetHidden(workbook.getSheetIndex(hidden), true);
        XSSFDataValidationHelper helper = new XSSFDataValidationHelper(mainSheet);
        DataValidationConstraint constraint = helper.createFormulaListConstraint(hiddenSheetName);
        CellRangeAddressList regions = new CellRangeAddressList(
                DATA_START_ROW_INDEX, DATA_START_ROW_INDEX + MAX_DATA_ROWS - 1, columnIndex, columnIndex);
        XSSFDataValidation validation = (XSSFDataValidation) helper.createValidation(constraint, regions);
        validation.setShowErrorBox(false);
        mainSheet.addValidationData(validation);
    }
}
