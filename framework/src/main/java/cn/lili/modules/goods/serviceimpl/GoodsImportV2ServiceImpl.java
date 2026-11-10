package cn.lili.modules.goods.serviceimpl;

import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.goods.entity.dos.Brand;
import cn.lili.modules.goods.entity.dos.Category;
import cn.lili.modules.goods.entity.dos.GoodsUnit;
import cn.lili.modules.goods.entity.dto.GoodsOperationDTO;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.entity.vo.GoodsImportResultVO;
import cn.lili.modules.goods.entity.vos.CategoryBrandVO;
import cn.lili.modules.goods.entity.vos.CategoryVO;
import cn.lili.modules.goods.entity.vos.StoreGoodsLabelVO;
import cn.lili.modules.goods.service.BrandService;
import cn.lili.modules.goods.service.CategoryBrandService;
import cn.lili.modules.goods.service.CategoryService;
import cn.lili.modules.goods.service.GoodsImportV2Service;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.modules.goods.service.GoodsUnitService;
import cn.lili.modules.goods.service.StoreGoodsLabelService;
import cn.lili.modules.store.entity.dos.StoreDetail;
import cn.lili.modules.store.entity.vos.FreightTemplateVO;
import cn.lili.modules.store.service.FreightTemplateService;
import cn.lili.modules.store.service.StoreDetailService;
import org.apache.poi.ss.usermodel.DataFormatter;
import org.apache.poi.ss.usermodel.DataValidationConstraint;
import org.apache.poi.ss.usermodel.Name;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.ss.util.CellRangeAddressList;
import org.apache.poi.xssf.usermodel.XSSFDataValidation;
import org.apache.poi.xssf.usermodel.XSSFDataValidationHelper;
import org.apache.poi.xssf.usermodel.XSSFRow;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;

/**
 * 商品批量导入v2业务层实现：解析多规格商品 Excel（第 1 行说明、第 2 行表头、第 3 行起数据），
 * 按「商品序号」列将同一商品的多个 SKU 行归并为一组，做序号预检（非法/空缺/复用序号转失败记录）
 * 与组内两级校验（行级 SKU 字段 + 商品级字段，含分类/品牌/运费模板/计量单位/店铺分类的归属校验），
 * 商品级字段按组内继承规则以首行为准；校验通过的组组装为 GoodsOperationDTO 逐个调用
 * goodsService.addGoods 落库（addGoods 自身事务即每商品独立事务，单组失败记 failList 后继续，
 * 实现部分成功）；downloadTemplate 用 XSSF 生成 .xlsx 模板（第 1 行合并说明、第 2 行 20 列表头、
 * 隐藏 sheet + 命名区域为平台分类/店铺分类/品牌/运费模板/计量单位列提供 id-名称 下拉）。
 *
 * @author Bulbasaur
 * @since 2026-10-20
 */
@Service
public class GoodsImportV2ServiceImpl implements GoodsImportV2Service {

    @Autowired
    private CategoryService categoryService;
    @Autowired
    private CategoryBrandService categoryBrandService;
    @Autowired
    private FreightTemplateService freightTemplateService;
    @Autowired
    private GoodsUnitService goodsUnitService;
    @Autowired
    private StoreGoodsLabelService storeGoodsLabelService;
    @Autowired
    private GoodsService goodsService;
    @Autowired
    private BrandService brandService;
    @Autowired
    private StoreDetailService storeDetailService;

    /**
     * 模板列数（A-T 共 20 列）
     */
    static final int COLUMN_COUNT = 20;

    /**
     * 模板第 2 行的 20 列表头文案，顺序与 {@link Row} 字段（A-T 列）一一对应，列错位会导致整列解析错位
     */
    static final String[] TEMPLATE_HEADERS = {
            "商品序号", "商品名称", "平台分类", "店铺分类", "品牌",
            "运费模板", "计量单位", "商品图片", "规格1名称", "规格1值",
            "规格2名称", "规格2值", "规格3名称", "规格3值", "SKU编码",
            "销售价", "成本价", "库存", "重量", "预警库存"};

    /**
     * 模板第 1 行（合并 A1:T1）的填写说明：分组规则、商品级字段只填首行、名称逗号与图片素材限制
     */
    static final String TEMPLATE_INSTRUCTION =
            "填写说明：1.一行一个SKU、同商品填相同序号（续行商品序号留空自动归属上一商品）；"
                    + "2.商品级字段只填首行（商品名称、平台分类、店铺分类、品牌、运费模板、计量单位、商品图片、规格名称），续行留空；"
                    + "3.商品名称勿含英文逗号；"
                    + "4.商品图片须先上传素材中心，再复制图片URL填写，多个用英文逗号分隔、首张为主图；"
                    + "5.平台分类、店铺分类、品牌、运费模板、计量单位可从下拉选择（id-名称格式）。";

    /**
     * 数据起始行下标（从 0 开始，跳过第 1 行说明与第 2 行表头）
     */
    static final int DATA_START_ROW_INDEX = 2;

    /**
     * 单次导入数据行上限（不含说明行与表头），超过即整文件拒绝
     */
    static final int MAX_DATA_ROWS = 2000;

    /**
     * 单次导入商品数量上限（按商品序号分组计数），超过即整文件拒绝
     */
    static final int MAX_GOODS_COUNT = 500;

    @Override
    public GoodsImportResultVO importGoods(MultipartFile file) {
        String filename = file.getOriginalFilename();
        if (filename == null || !filename.toLowerCase(Locale.ROOT).endsWith(".xlsx")) {
            throw new ServiceException("仅支持上传 .xlsx 格式的商品导入文件");
        }
        List<Row> rows;
        try (InputStream in = file.getInputStream()) {
            rows = parseRows(in);
        } catch (IOException e) {
            throw new ServiceException("商品导入文件读取失败：" + e.getMessage());
        } catch (RuntimeException e) {
            // POI 对损坏/伪装的 xlsx（如改后缀的非 Excel 文件）会抛 RuntimeException，收敛为友好提示
            throw new ServiceException("商品导入文件解析失败，请使用下载的模板重新填写后导入");
        }
        if (rows.size() > MAX_DATA_ROWS) {
            throw new ServiceException("单次最多导入" + MAX_DATA_ROWS + "行数据，当前文件共" + rows.size() + "行，请拆分后重试");
        }
        GroupingResult grouping = splitValidGroups(rows);
        if (grouping.groups.size() > MAX_GOODS_COUNT) {
            throw new ServiceException("单次最多导入" + MAX_GOODS_COUNT + "个商品（按商品序号计），当前文件共"
                    + grouping.groups.size() + "个，请拆分后重试");
        }
        GoodsImportResultVO result = new GoodsImportResultVO();
        result.getFailList().addAll(grouping.fails);
        for (Map.Entry<Integer, List<Row>> entry : grouping.groups.entrySet()) {
            Integer goodsNo = entry.getKey();
            List<Row> groupRows = entry.getValue();
            String error = validateGroup(goodsNo, groupRows);
            if (error == null) {
                try {
                    goodsService.addGoods(buildGoodsOperationDTO(groupRows));
                    result.setSuccessCount(result.getSuccessCount() + 1);
                    continue;
                } catch (Exception e) {
                    error = e.getMessage() == null ? "商品保存失败" : e.getMessage();
                }
            }
            GoodsImportResultVO.FailGoods failGoods = buildFailGoods(groupRows, error);
            failGoods.setGoodsNo(goodsNo);
            result.getFailList().add(failGoods);
        }
        result.setFailCount(result.getFailList().size());
        return result;
    }

    /**
     * 将校验通过（已完成组内继承回填）的商品组组装为 GoodsOperationDTO。
     * 关键规则：
     * ① skuList 每条 map 含 price/cost/quantity/weight/alertQuantity、规格名→规格值动态键、
     * images（首行图片数组）；不放 sn 键，由 GoodsSkuBuilder 兜底生成以规避批量 SN 碰撞；
     * ② 预警库存留空时按 0 写入（校验层只放行不回填，默认值在组装层补齐）；
     * ③ DTO 的 price 取组内 SKU 最低价；categoryPath 为回溯出的三级分类 ID 逗号串；
     * templateId/brandId/storeCategoryPath 解析为对应记录 ID，goodsUnit 存单位名称
     * （Excel 允许填 ID、名称或 ID-名称，统一归一到落库口径）；
     * ④ 导入商品一律下架暂存：release=false、goodsType=PHYSICAL_GOODS、intro/mobileIntro 为空串、
     * goodsParamsDTOList 为 null（不触碰商品参数与倒排表）。
     *
     * @param rows 校验通过的商品组数据行（首行为商品级字段来源）
     * @return 可直接交给 goodsService.addGoods 的商品操作 DTO
     */
    private GoodsOperationDTO buildGoodsOperationDTO(List<Row> rows) {
        Row first = rows.get(0);
        List<String> gallery = splitImages(first.goodsImages);
        List<Map<String, Object>> skuList = new ArrayList<>(rows.size());
        double minPrice = Double.MAX_VALUE;
        for (Row row : rows) {
            Map<String, Object> sku = new LinkedHashMap<>();
            double price = Double.parseDouble(row.price);
            sku.put("price", price);
            sku.put("cost", Double.parseDouble(row.cost));
            sku.put("quantity", Integer.valueOf(row.quantity));
            sku.put("weight", Double.parseDouble(row.weight));
            sku.put("alertQuantity", row.alertQuantity.isEmpty() ? 0 : Integer.valueOf(row.alertQuantity));
            if (!row.spec1Name.isEmpty()) {
                sku.put(row.spec1Name, row.spec1Value);
            }
            if (!row.spec2Name.isEmpty()) {
                sku.put(row.spec2Name, row.spec2Value);
            }
            if (!row.spec3Name.isEmpty()) {
                sku.put(row.spec3Name, row.spec3Value);
            }
            sku.put("images", gallery);
            skuList.add(sku);
            minPrice = Math.min(minPrice, price);
        }
        GoodsOperationDTO dto = new GoodsOperationDTO();
        dto.setGoodsName(first.goodsName);
        dto.setPrice(minPrice);
        dto.setCategoryPath(resolveCategoryPath(first));
        dto.setStoreCategoryPath(resolveStoreCategoryPath(first.storeCategory));
        dto.setBrandId(resolveBrandId(first));
        dto.setTemplateId(resolveTemplateId(first.freightTemplate));
        dto.setGoodsUnit(resolveGoodsUnitName(first.goodsUnit));
        dto.setGoodsGalleryList(gallery);
        dto.setSkuList(skuList);
        dto.setIntro("");
        dto.setMobileIntro("");
        dto.setGoodsType(GoodsTypeEnum.PHYSICAL_GOODS.name());
        dto.setRelease(false);
        dto.setRecommend(false);
        dto.setGoodsParamsDTOList(null);
        return dto;
    }

    /**
     * 拆分商品图片列为 URL 列表（英文逗号分隔，逐项去空白）。
     *
     * @param goodsImages 图片列原始文本（校验已保证非空且均为 http(s):// 前缀）
     * @return 图片 URL 列表，首张为主图
     */
    private static List<String> splitImages(String goodsImages) {
        List<String> images = new ArrayList<>();
        for (String url : goodsImages.split(",")) {
            String image = url.trim();
            if (!image.isEmpty()) {
                images.add(image);
            }
        }
        return images;
    }

    /**
     * 截取平台分类填值的分类 ID（「分类ID-名称」取首个「-」之前；纯 ID 原样返回）。
     *
     * @param categoryPath 平台分类列原始填值
     * @return 分类 ID
     */
    private static String categoryIdOf(String categoryPath) {
        return categoryPath.contains("-")
                ? categoryPath.substring(0, categoryPath.indexOf('-')) : categoryPath;
    }

    /**
     * 回溯平台分类的三级分类路径（一级,二级,三级 ID 逗号串）；调用前校验已保证三级完整。
     *
     * @param first 组内首行
     * @return 三级分类 ID 逗号串
     */
    private String resolveCategoryPath(Row first) {
        Category leaf = categoryService.getCategoryById(categoryIdOf(first.categoryPath));
        Category second = categoryService.getCategoryById(leaf.getParentId());
        Category top = categoryService.getCategoryById(second.getParentId());
        return top.getId() + "," + second.getId() + "," + leaf.getId();
    }

    /**
     * 解析运费模板填值为本店运费模板 ID；校验已保证命中本店模板列表。
     *
     * @param freightTemplate 运费模板列填值（ID / 名称 / ID-名称）
     * @return 运费模板 ID
     */
    private String resolveTemplateId(String freightTemplate) {
        FreightTemplateVO template = findOption(freightTemplate,
                freightTemplateService.getFreightTemplateList(currentStoreId()),
                FreightTemplateVO::getId, FreightTemplateVO::getName);
        return template == null ? freightTemplate : template.getId();
    }

    /**
     * 解析品牌填值为品牌 ID；未填品牌返回 null。
     *
     * @param first 组内首行
     * @return 品牌 ID，未填时返回 null
     */
    private String resolveBrandId(Row first) {
        if (first.brand.isEmpty()) {
            return null;
        }
        CategoryBrandVO brand = findOption(first.brand,
                categoryBrandService.getCategoryBrandList(categoryIdOf(first.categoryPath)),
                CategoryBrandVO::getId, CategoryBrandVO::getName);
        return brand == null ? first.brand : brand.getId();
    }

    /**
     * 解析计量单位填值为单位名称（GoodsOperationDTO.goodsUnit 存名称）；校验已保证命中平台单位表。
     *
     * @param goodsUnit 计量单位列填值（ID / 名称 / ID-名称）
     * @return 计量单位名称
     */
    private String resolveGoodsUnitName(String goodsUnit) {
        GoodsUnit unit = findOption(goodsUnit, goodsUnitService.list(), GoodsUnit::getId, GoodsUnit::getName);
        return unit == null ? goodsUnit : unit.getName();
    }

    /**
     * 解析店铺分类填值（逗号分隔多项）为本店店铺分类 ID 逗号串；未填返回 null。
     *
     * @param storeCategory 店铺分类列填值
     * @return 店铺分类 ID 逗号串，未填时返回 null
     */
    private String resolveStoreCategoryPath(String storeCategory) {
        if (storeCategory.isEmpty()) {
            return null;
        }
        List<StoreGoodsLabelVO> labels = storeGoodsLabelService.listByStoreId(currentStoreId());
        List<String> ids = new ArrayList<>();
        for (String item : storeCategory.split(",")) {
            String label = item.trim();
            if (label.isEmpty()) {
                continue;
            }
            StoreGoodsLabelVO option = findOption(label, labels, StoreGoodsLabelVO::getId, StoreGoodsLabelVO::getLabelName);
            ids.add(option == null ? label : option.getId());
        }
        return ids.isEmpty() ? null : String.join(",", ids);
    }

    @Override
    public void downloadTemplate(HttpServletResponse response) {
        String storeId = currentStoreId();
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
            addDropdown(workbook, sheet, "hiddenCategory", categoryOptions(storeId), 2);
            addDropdown(workbook, sheet, "hiddenStoreCategory", storeCategoryOptions(storeId), 3);
            addDropdown(workbook, sheet, "hiddenBrand", brandOptions(), 4);
            addDropdown(workbook, sheet, "hiddenFreightTemplate", freightTemplateOptions(storeId), 5);
            addDropdown(workbook, sheet, "hiddenGoodsUnit", goodsUnitOptions(), 6);
            response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
            response.setHeader("Content-Disposition",
                    "attachment;filename=" + URLEncoder.encode("商品导入模板.xlsx", "UTF-8"));
            workbook.write(response.getOutputStream());
        } catch (IOException e) {
            throw new ServiceException("商品导入模板生成失败：" + e.getMessage());
        }
    }

    /**
     * 收集平台分类下拉数据源：按店铺经营类目取分类树，遍历到第三级叶子，拼为「分类ID-名称」。
     *
     * @param storeId 当前店铺 ID
     * @return 叶子三级分类的 id-名称 列表；店铺经营类目未配置时为空列表
     */
    private List<String> categoryOptions(String storeId) {
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
     * @param storeId 当前店铺 ID
     * @return 店铺分类的 id-名称 列表
     */
    private List<String> storeCategoryOptions(String storeId) {
        List<String> options = new ArrayList<>();
        for (StoreGoodsLabelVO label : storeGoodsLabelService.listByStoreId(storeId)) {
            options.add(label.getId() + "-" + label.getLabelName());
        }
        return options;
    }

    /**
     * 收集品牌下拉数据源：静态模板无法按所选分类联动，放全量品牌「ID-名称」供选择，
     * 品牌与分类的归属关系由导入校验层拦截。
     *
     * @return 全量品牌的 id-名称 列表
     */
    private List<String> brandOptions() {
        List<String> options = new ArrayList<>();
        for (Brand brand : brandService.listNotDeleted()) {
            options.add(brand.getId() + "-" + brand.getName());
        }
        return options;
    }

    /**
     * 收集运费模板下拉数据源（本店运费模板，「ID-名称」）。
     *
     * @param storeId 当前店铺 ID
     * @return 运费模板的 id-名称 列表
     */
    private List<String> freightTemplateOptions(String storeId) {
        List<String> options = new ArrayList<>();
        for (FreightTemplateVO template : freightTemplateService.getFreightTemplateList(storeId)) {
            options.add(template.getId() + "-" + template.getName());
        }
        return options;
    }

    /**
     * 收集计量单位下拉数据源（平台计量单位，「ID-名称」）。
     *
     * @return 计量单位的 id-名称 列表
     */
    private List<String> goodsUnitOptions() {
        List<String> options = new ArrayList<>();
        for (GoodsUnit goodsUnit : goodsUnitService.list()) {
            options.add(goodsUnit.getId() + "-" + goodsUnit.getName());
        }
        return options;
    }

    /**
     * 为模板主 sheet 的某一列添加下拉：候选项写入隐藏 sheet 的 A 列，通过命名区域引用，
     * 再用 XSSFDataValidation 绑定到数据行区域（第 3 行起，覆盖数据行上限）。
     *
     * @param workbook       工作簿
     * @param mainSheet      模板主 sheet
     * @param hiddenSheetName 隐藏 sheet 名（同时作为命名区域名，仅用 ASCII 避免公式转义）
     * @param options        下拉候选项（id-名称）
     * @param columnIndex    主 sheet 目标列下标（从 0 开始）
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

    /**
     * 解析 Excel 首个 sheet 的数据行：跳过前 2 行（说明+表头），整行空白跳过；
     * 列值用 DataFormatter 取显示文本，避免数字单元格读出科学计数或多余小数。
     *
     * @param in Excel 文件输入流（xlsx/xls 均可，由 WorkbookFactory 识别）
     * @return 数据行列表，按 Excel 行顺序排列
     * @throws IOException 读取文件失败时抛出
     */
    static List<Row> parseRows(InputStream in) throws IOException {
        List<Row> rows = new ArrayList<>();
        DataFormatter formatter = new DataFormatter();
        try (Workbook workbook = WorkbookFactory.create(in)) {
            Sheet sheet = workbook.getSheetAt(0);
            for (int i = DATA_START_ROW_INDEX; i <= sheet.getLastRowNum(); i++) {
                org.apache.poi.ss.usermodel.Row excelRow = sheet.getRow(i);
                if (excelRow == null) {
                    continue;
                }
                String[] cells = new String[COLUMN_COUNT];
                boolean blank = true;
                for (int c = 0; c < COLUMN_COUNT; c++) {
                    cells[c] = formatter.formatCellValue(excelRow.getCell(c)).trim();
                    if (!cells[c].isEmpty()) {
                        blank = false;
                    }
                }
                if (blank) {
                    continue;
                }
                rows.add(new Row(cells, i + 1));
            }
        }
        return rows;
    }

    /**
     * 按商品序号分组：商品序号（第 A 列）非空时开启新组，留空时作为续行归入最近的组；
     * 组内与组间均保持原始行顺序。
     * 注意：本方法假定序号合法，非法序号（非数字、首行留空、复用/回退）须先经
     * {@link #splitValidGroups(List)} 预检剔除，否则会抛 NumberFormatException 或静默丢行。
     *
     * @param rows 解析后的数据行
     * @return 商品序号到行列表的映射，key 为商品序号
     */
    static Map<Integer, List<Row>> groupByGoodsNo(List<Row> rows) {
        Map<Integer, List<Row>> groups = new LinkedHashMap<>();
        Integer currentGoodsNo = null;
        for (Row row : rows) {
            if (!row.goodsNo.isEmpty()) {
                currentGoodsNo = Integer.valueOf(row.goodsNo.trim());
            }
            if (currentGoodsNo == null) {
                continue;
            }
            groups.computeIfAbsent(currentGoodsNo, k -> new ArrayList<>()).add(row);
        }
        return groups;
    }

    /**
     * 序号预检并分组：在 {@link #groupByGoodsNo(List)} 之前拦截三类非法序号行，避免分组阶段
     * 抛 NumberFormatException 或静默丢行——
     * ①商品序号非数字；②文件首个有效组之前商品序号留空（无所属组）；③序号复用（同一序号非连续
     * 出现）或序号回退（小于上一组序号，防止商家误把两个商品填成同号）。
     * 非法段连同其续行整体转为一条失败记录（失败粒度为商品组），合法组保持原始行顺序。
     *
     * @param rows 解析后的数据行
     * @return 合法分组与序号预检失败记录
     */
    static GroupingResult splitValidGroups(List<Row> rows) {
        GroupingResult result = new GroupingResult();
        Set<Integer> seenGoodsNo = new HashSet<>();
        Integer lastGoodsNo = null;
        Integer currentGoodsNo = null;
        List<Row> invalidRows = null;
        String invalidReason = null;
        for (Row row : rows) {
            if (!row.goodsNo.isEmpty()) {
                if (invalidRows != null) {
                    result.fails.add(buildFailGoods(invalidRows, invalidReason));
                    invalidRows = null;
                    invalidReason = null;
                }
                currentGoodsNo = null;
                Integer goodsNo = parseGoodsNo(row.goodsNo);
                if (goodsNo == null) {
                    invalidReason = "第" + row.excelRowNo + "行：商品序号必须为数字，当前值：" + row.goodsNo;
                } else if (seenGoodsNo.contains(goodsNo)) {
                    invalidReason = "第" + row.excelRowNo + "行：商品序号" + goodsNo + "已在前文使用，同一商品序号必须连续填写";
                } else if (lastGoodsNo != null && goodsNo < lastGoodsNo) {
                    invalidReason = "第" + row.excelRowNo + "行：商品序号" + goodsNo + "小于上一组序号" + lastGoodsNo + "，请按递增顺序填写";
                } else {
                    currentGoodsNo = goodsNo;
                    seenGoodsNo.add(goodsNo);
                    lastGoodsNo = goodsNo;
                }
                if (currentGoodsNo == null) {
                    invalidRows = new ArrayList<>();
                    invalidRows.add(row);
                    continue;
                }
                result.groups.computeIfAbsent(currentGoodsNo, k -> new ArrayList<>()).add(row);
            } else if (currentGoodsNo != null) {
                result.groups.get(currentGoodsNo).add(row);
            } else {
                if (invalidRows == null) {
                    invalidRows = new ArrayList<>();
                    invalidReason = "第" + row.excelRowNo + "行：商品序号不能为空，且其上方没有可归属的商品组";
                }
                invalidRows.add(row);
            }
        }
        if (invalidRows != null) {
            result.fails.add(buildFailGoods(invalidRows, invalidReason));
        }
        return result;
    }

    /**
     * 校验一个商品组并做组内继承：先将首行的商品级字段（名称、分类、店铺分类、品牌、运费模板、
     * 计量单位、图片、规格名称）继承到留空的续行（填写了的续行以首行为准直接忽略），再做两级校验。
     * 行级：销售价/成本价 &gt;0 且最多两位小数、库存非负整数、重量 ≥0、预警库存非负整数、
     * 规格名/值成对填写、组内规格值组合与 SKU 编码去重；
     * 商品级：名称非空 ≤50 字、图片非空且均为 http(s):// 前缀、平台分类（id-名称，截「-」前 ID）
     * 存在且可回溯出三级分类、品牌属于所选分类、运费模板/店铺分类属于当前登录店铺（越权拦截）、
     * 计量单位存在于平台单位表。任一规则不过则整组失败。
     *
     * @param goodsNo 商品序号（Excel 第 A 列分组标识）
     * @param rows    该商品组的全部数据行，首行为商品级字段来源
     * @return null 表示校验通过，否则返回含行号的失败原因
     */
    String validateGroup(Integer goodsNo, List<Row> rows) {
        if (goodsNo == null) {
            return "商品序号不能为空";
        }
        if (rows == null || rows.isEmpty()) {
            return "商品序号" + goodsNo + "没有数据行";
        }
        Row first = rows.get(0);
        for (int i = 1; i < rows.size(); i++) {
            inheritProductFields(first, rows.get(i));
        }
        String rowError = validateSkuRows(rows);
        if (rowError != null) {
            return rowError;
        }
        return validateGoodsFields(first);
    }

    /**
     * 组内继承：将首行商品级字段值填充到续行的留空字段；续行已填写的商品级字段以首行为准，被首行值覆盖。
     *
     * @param first 组内首行（商品级字段来源）
     * @param row   待继承的续行
     */
    private static void inheritProductFields(Row first, Row row) {
        row.goodsName = first.goodsName;
        row.categoryPath = first.categoryPath;
        row.storeCategory = first.storeCategory;
        row.brand = first.brand;
        row.freightTemplate = first.freightTemplate;
        row.goodsUnit = first.goodsUnit;
        row.goodsImages = first.goodsImages;
        row.spec1Name = first.spec1Name;
        row.spec2Name = first.spec2Name;
        row.spec3Name = first.spec3Name;
    }

    /**
     * 行级（SKU 级）校验：逐行检查价格、库存、重量及规格/SKU 编码的组内唯一性。
     *
     * @param rows 已完成组内继承的商品组数据行
     * @return null 表示通过，否则返回含行号的失败原因
     */
    private static String validateSkuRows(List<Row> rows) {
        Map<String, Integer> specComboRows = new HashMap<>();
        Map<String, Integer> skuSnRows = new HashMap<>();
        for (Row row : rows) {
            String error = checkPositiveMoney(row.price, "销售价", row.excelRowNo);
            if (error == null) {
                error = checkPositiveMoney(row.cost, "成本价", row.excelRowNo);
            }
            if (error == null) {
                error = checkNonNegativeInteger(row.quantity, "库存", true, row.excelRowNo);
            }
            if (error == null) {
                error = checkWeight(row.weight, row.excelRowNo);
            }
            if (error == null) {
                error = checkNonNegativeInteger(row.alertQuantity, "预警库存", false, row.excelRowNo);
            }
            if (error == null) {
                error = checkSpecPair(row.spec1Name, row.spec1Value, 1, row.excelRowNo);
            }
            if (error == null) {
                error = checkSpecPair(row.spec2Name, row.spec2Value, 2, row.excelRowNo);
            }
            if (error == null) {
                error = checkSpecPair(row.spec3Name, row.spec3Value, 3, row.excelRowNo);
            }
            if (error != null) {
                return error;
            }
            // 规格值组合以“ ”分隔拼接，避免「AB+C」与「A+BC」这类值文本误判为重复
            String combo = row.spec1Value + '' + row.spec2Value + '' + row.spec3Value;
            Integer comboRowNo = specComboRows.putIfAbsent(combo, row.excelRowNo);
            if (comboRowNo != null) {
                return "第" + row.excelRowNo + "行：规格值组合与第" + comboRowNo + "行重复";
            }
            if (!row.skuSn.isEmpty()) {
                Integer snRowNo = skuSnRows.putIfAbsent(row.skuSn, row.excelRowNo);
                if (snRowNo != null) {
                    return "第" + row.excelRowNo + "行：SKU编码与第" + snRowNo + "行重复：" + row.skuSn;
                }
            }
        }
        return null;
    }

    /**
     * 商品级校验：以组内首行为准校验名称、图片、平台分类、品牌、运费模板、计量单位与店铺分类。
     *
     * @param first 组内首行
     * @return null 表示通过，否则返回含行号的失败原因
     */
    private String validateGoodsFields(Row first) {
        int rowNo = first.excelRowNo;
        if (first.goodsName.isEmpty()) {
            return "第" + rowNo + "行：商品名称不能为空";
        }
        if (first.goodsName.length() > 50) {
            return "第" + rowNo + "行：商品名称不能超过50字";
        }
        if (first.goodsImages.isEmpty()) {
            return "第" + rowNo + "行：商品图片不能为空";
        }
        for (String url : first.goodsImages.split(",")) {
            String image = url.trim();
            if (!image.startsWith("http://") && !image.startsWith("https://")) {
                return "第" + rowNo + "行：商品图片须以http://或https://开头：" + url.trim();
            }
        }
        if (first.categoryPath.isEmpty()) {
            return "第" + rowNo + "行：平台分类不能为空";
        }
        String categoryId = categoryIdOf(first.categoryPath);
        String categoryError = checkCategoryPath(categoryId, first.categoryPath, rowNo);
        if (categoryError != null) {
            return categoryError;
        }
        if (!first.brand.isEmpty()) {
            List<CategoryBrandVO> brands = categoryBrandService.getCategoryBrandList(categoryId);
            if (!matchesOption(first.brand, brands, CategoryBrandVO::getId, CategoryBrandVO::getName)) {
                return "第" + rowNo + "行：品牌不属于所选分类：" + first.brand;
            }
        }
        String storeId = currentStoreId();
        if (storeId == null) {
            return "当前登录用户不是商家，无法导入商品";
        }
        if (first.freightTemplate.isEmpty()) {
            return "第" + rowNo + "行：运费模板不能为空";
        }
        List<FreightTemplateVO> freightTemplates = freightTemplateService.getFreightTemplateList(storeId);
        if (!matchesOption(first.freightTemplate, freightTemplates, FreightTemplateVO::getId, FreightTemplateVO::getName)) {
            return "第" + rowNo + "行：运费模板不存在或不属于当前店铺：" + first.freightTemplate;
        }
        if (first.goodsUnit.isEmpty()) {
            return "第" + rowNo + "行：计量单位不能为空";
        }
        if (!matchesOption(first.goodsUnit, goodsUnitService.list(), GoodsUnit::getId, GoodsUnit::getName)) {
            return "第" + rowNo + "行：计量单位不存在：" + first.goodsUnit;
        }
        if (!first.storeCategory.isEmpty()) {
            List<StoreGoodsLabelVO> labels = storeGoodsLabelService.listByStoreId(storeId);
            for (String item : first.storeCategory.split(",")) {
                String label = item.trim();
                if (label.isEmpty()) {
                    continue;
                }
                if (!matchesOption(label, labels, StoreGoodsLabelVO::getId, StoreGoodsLabelVO::getLabelName)) {
                    return "第" + rowNo + "行：店铺分类不存在或不属于当前店铺：" + label;
                }
            }
        }
        return null;
    }

    /**
     * 校验平台分类存在且可沿 parentId 回溯出完整的三级分类路径。
     *
     * @param categoryId 截「-」前的分类 ID
     * @param rawValue   Excel 原始填值，用于失败原因展示
     * @param rowNo      首行行号
     * @return null 表示通过，否则返回失败原因
     */
    private String checkCategoryPath(String categoryId, String rawValue, int rowNo) {
        Category leaf = categoryService.getCategoryById(categoryId);
        if (leaf == null) {
            return "第" + rowNo + "行：平台分类不存在：" + rawValue;
        }
        Category second = leaf.getParentId() == null ? null : categoryService.getCategoryById(leaf.getParentId());
        Category top = second == null || second.getParentId() == null ? null : categoryService.getCategoryById(second.getParentId());
        if (second == null || top == null) {
            return "第" + rowNo + "行：平台分类须为三级分类：" + rawValue;
        }
        return null;
    }

    /**
     * 校验金额字段：必填、&gt;0 且最多两位小数（尾随的 0 不计入小数位）。
     *
     * @param value      单元格文本
     * @param fieldLabel 字段中文名
     * @param rowNo      行号
     * @return null 表示通过，否则返回含行号的失败原因
     */
    private static String checkPositiveMoney(String value, String fieldLabel, int rowNo) {
        if (value.isEmpty()) {
            return "第" + rowNo + "行：" + fieldLabel + "不能为空";
        }
        BigDecimal amount;
        try {
            amount = new BigDecimal(value);
        } catch (NumberFormatException e) {
            return "第" + rowNo + "行：" + fieldLabel + "格式不正确：" + value;
        }
        if (amount.compareTo(BigDecimal.ZERO) <= 0) {
            return "第" + rowNo + "行：" + fieldLabel + "必须大于0";
        }
        if (amount.stripTrailingZeros().scale() > 2) {
            return "第" + rowNo + "行：" + fieldLabel + "最多两位小数";
        }
        return null;
    }

    /**
     * 校验非负整数字段（库存必填；预警库存选填，留空即通过——本层只校验不回填，
     * 预警库存留空的默认值 0 由组装层 {@link #buildGoodsOperationDTO(List)} 写入）。
     *
     * @param value      单元格文本
     * @param fieldLabel 字段中文名
     * @param required   是否必填
     * @param rowNo      行号
     * @return null 表示通过，否则返回含行号的失败原因
     */
    private static String checkNonNegativeInteger(String value, String fieldLabel, boolean required, int rowNo) {
        if (value.isEmpty()) {
            return required ? "第" + rowNo + "行：" + fieldLabel + "不能为空" : null;
        }
        if (!value.matches("\\d+")) {
            return "第" + rowNo + "行：" + fieldLabel + "必须为非负整数";
        }
        return null;
    }

    /**
     * 校验重量字段：必填（导入商品均为实物）、数字且 ≥0，单位 kg。
     *
     * @param value 单元格文本
     * @param rowNo 行号
     * @return null 表示通过，否则返回含行号的失败原因
     */
    private static String checkWeight(String value, int rowNo) {
        if (value.isEmpty()) {
            return "第" + rowNo + "行：重量不能为空";
        }
        try {
            if (new BigDecimal(value).compareTo(BigDecimal.ZERO) < 0) {
                return "第" + rowNo + "行：重量不能小于0";
            }
        } catch (NumberFormatException e) {
            return "第" + rowNo + "行：重量格式不正确：" + value;
        }
        return null;
    }

    /**
     * 校验规格名称与规格值成对填写：名称非空时值必填，值非空时名称须已在首行填写。
     *
     * @param specName  规格名称（组内继承后的商品级字段）
     * @param specValue 规格值（SKU 级字段）
     * @param index     规格序号（1-3）
     * @param rowNo     行号
     * @return null 表示通过，否则返回含行号的失败原因
     */
    private static String checkSpecPair(String specName, String specValue, int index, int rowNo) {
        if (!specName.isEmpty() && specValue.isEmpty()) {
            return "第" + rowNo + "行：规格" + index + "值不能为空";
        }
        if (specName.isEmpty() && !specValue.isEmpty()) {
            return "第" + rowNo + "行：规格" + index + "名称未在首行填写，不能填写规格" + index + "值";
        }
        return null;
    }

    /**
     * 判断填值是否命中候选列表中的某一项：接受「ID」「名称」或模板下拉使用的「ID-名称」三种写法。
     *
     * @param value     Excel 填值
     * @param options   候选列表（分类品牌/运费模板/计量单位/店铺分类）
     * @param idGetter  候选项 ID 取值函数
     * @param nameGetter 候选项名称取值函数
     * @param <T>       候选项类型
     * @return 命中返回 true
     */
    private static <T> boolean matchesOption(String value, List<T> options, Function<T, String> idGetter, Function<T, String> nameGetter) {
        return findOption(value, options, idGetter, nameGetter) != null;
    }

    /**
     * 在候选列表中查找填值命中的候选项，命中规则同 {@link #matchesOption}；供组装层把填值归一为 ID/名称。
     *
     * @param value      Excel 填值
     * @param options    候选列表（分类品牌/运费模板/计量单位/店铺分类）
     * @param idGetter   候选项 ID 取值函数
     * @param nameGetter 候选项名称取值函数
     * @param <T>        候选项类型
     * @return 命中的候选项，未命中返回 null
     */
    private static <T> T findOption(String value, List<T> options, Function<T, String> idGetter, Function<T, String> nameGetter) {
        if (options == null) {
            return null;
        }
        for (T option : options) {
            String id = idGetter.apply(option);
            String name = nameGetter.apply(option);
            if (value.equals(id) || value.equals(name) || (id != null && name != null && value.equals(id + "-" + name))) {
                return option;
            }
        }
        return null;
    }

    /**
     * 获取当前登录商家店铺 ID，非商家角色或未登录返回 null。
     *
     * @return 当前店铺 ID，无商家上下文时返回 null
     */
    private static String currentStoreId() {
        AuthUser user = UserContext.getCurrentUser();
        if (user == null || user.getRole() != UserEnums.STORE || user.getStoreId() == null) {
            return null;
        }
        return user.getStoreId();
    }

    /**
     * 解析商品序号，非数字时返回 null 而非抛 NumberFormatException。
     *
     * @param goodsNo 商品序号单元格文本
     * @return 数字序号，非法时返回 null
     */
    private static Integer parseGoodsNo(String goodsNo) {
        try {
            return Integer.valueOf(goodsNo.trim());
        } catch (NumberFormatException e) {
            return null;
        }
    }

    /**
     * 将一段序号非法的行构建为一条失败记录（失败粒度为商品组，行范围为该段首末行号）。
     *
     * @param rows   非法段的全部行
     * @param reason 失败原因
     * @return 失败商品记录
     */
    private static GoodsImportResultVO.FailGoods buildFailGoods(List<Row> rows, String reason) {
        GoodsImportResultVO.FailGoods failGoods = new GoodsImportResultVO.FailGoods();
        int firstRowNo = rows.get(0).excelRowNo;
        int lastRowNo = rows.get(rows.size() - 1).excelRowNo;
        failGoods.setRows(firstRowNo == lastRowNo ? String.valueOf(firstRowNo) : firstRowNo + "-" + lastRowNo);
        failGoods.setReason(reason);
        return failGoods;
    }

    /**
     * 序号预检结果：合法的商品分组与因序号非法（非数字/空缺/复用/回退）产生的失败记录
     *
     * @author Bulbasaur
     * @since 2026-10-20
     */
    static class GroupingResult {

        /**
         * 通过序号预检的商品分组，key 为商品序号，保持原始行顺序
         */
        final Map<Integer, List<Row>> groups = new LinkedHashMap<>();

        /**
         * 序号预检失败的商品组明细，goodsNo 为 null（序号本身非法无法解析）
         */
        final List<GoodsImportResultVO.FailGoods> fails = new ArrayList<>();
    }

    /**
     * 导入数据行，保存 20 列原始字符串（A-T 列）及 Excel 实际行号（从 1 开始）。
     * 商品级字段（B-I 列与规格名称列）非 final：校验阶段按组内继承规则以首行值回填续行。
     *
     * @author Bulbasaur
     * @since 2026-10-20
     */
    static class Row {

        /**
         * Excel 实际行号（从 1 开始），用于失败明细定位
         */
        final int excelRowNo;

        /**
         * A 列：商品序号，同一商品的所有 SKU 行填相同序号，仅批次内有效；续行可留空表示归属上一组
         */
        final String goodsNo;

        /**
         * B 列：商品名称，商品级字段，只在同组首行填写
         */
        String goodsName;

        /**
         * C 列：平台分类，格式「分类ID-名称」（模板下拉生成，校验时截「-」前 ID 回溯三级分类），商品级字段
         */
        String categoryPath;

        /**
         * D 列：店铺分类，多个用英文逗号分隔，商品级字段
         */
        String storeCategory;

        /**
         * E 列：品牌，商品级字段
         */
        String brand;

        /**
         * F 列：运费模板，商品级字段
         */
        String freightTemplate;

        /**
         * G 列：计量单位，商品级字段
         */
        String goodsUnit;

        /**
         * H 列：商品图片 URL，多个英文逗号分隔，首张为主图，商品级字段
         */
        String goodsImages;

        /**
         * I 列：规格1名称，商品级字段，只在同组首行填写
         */
        String spec1Name;

        /**
         * J 列：规格1值，SKU 级字段
         */
        final String spec1Value;

        /**
         * K 列：规格2名称，商品级字段，只在同组首行填写
         */
        String spec2Name;

        /**
         * L 列：规格2值，SKU 级字段
         */
        final String spec2Value;

        /**
         * M 列：规格3名称，商品级字段，只在同组首行填写
         */
        String spec3Name;

        /**
         * N 列：规格3值，SKU 级字段
         */
        final String spec3Value;

        /**
         * O 列：SKU编码，选填，留空由后端生成
         */
        final String skuSn;

        /**
         * P 列：销售价，单位元，SKU 级字段
         */
        final String price;

        /**
         * Q 列：成本价，单位元，SKU 级字段
         */
        final String cost;

        /**
         * R 列：库存，SKU 级字段
         */
        final String quantity;

        /**
         * S 列：重量，单位 kg，SKU 级字段
         */
        final String weight;

        /**
         * T 列：预警库存，SKU 级字段
         */
        final String alertQuantity;

        /**
         * 按列数组构造数据行
         *
         * @param cells      20 列原始字符串，顺序对应 A-T 列
         * @param excelRowNo Excel 实际行号（从 1 开始）
         */
        Row(String[] cells, int excelRowNo) {
            this.goodsNo = cells[0];
            this.goodsName = cells[1];
            this.categoryPath = cells[2];
            this.storeCategory = cells[3];
            this.brand = cells[4];
            this.freightTemplate = cells[5];
            this.goodsUnit = cells[6];
            this.goodsImages = cells[7];
            this.spec1Name = cells[8];
            this.spec1Value = cells[9];
            this.spec2Name = cells[10];
            this.spec2Value = cells[11];
            this.spec3Name = cells[12];
            this.spec3Value = cells[13];
            this.skuSn = cells[14];
            this.price = cells[15];
            this.cost = cells[16];
            this.quantity = cells[17];
            this.weight = cells[18];
            this.alertQuantity = cells[19];
            this.excelRowNo = excelRowNo;
        }
    }
}
