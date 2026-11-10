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
import cn.lili.modules.goods.service.GoodsImportV2Service;
import cn.lili.modules.goods.service.GoodsUnitService;
import cn.lili.modules.goods.service.StoreGoodsLabelService;
import cn.lili.modules.store.entity.vos.FreightTemplateVO;
import cn.lili.modules.store.service.FreightTemplateService;
import org.apache.poi.ss.usermodel.DataFormatter;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;

/**
 * 商品批量导入v2业务层实现：解析多规格商品 Excel（第 1 行说明、第 2 行表头、第 3 行起数据），
 * 按「商品序号」列将同一商品的多个 SKU 行归并为一组，校验与落库在后续任务叠加。
 * 本类当前实现行解析、分组、序号预检（非法/空缺/复用序号转失败记录）与组内两级校验
 * （行级 SKU 字段 + 商品级字段，含分类/品牌/运费模板/计量单位/店铺分类的归属校验），
 * 商品级字段按组内继承规则以首行为准；importGoods/downloadTemplate 主流程待后续任务补全。
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

    /**
     * 模板列数（A-T 共 20 列）
     */
    static final int COLUMN_COUNT = 20;

    /**
     * 数据起始行下标（从 0 开始，跳过第 1 行说明与第 2 行表头）
     */
    static final int DATA_START_ROW_INDEX = 2;

    @Override
    public GoodsImportResultVO importGoods(MultipartFile file) {
        throw new UnsupportedOperationException("商品批量导入v2主流程将在后续任务实现");
    }

    @Override
    public void downloadTemplate(HttpServletResponse response) {
        throw new UnsupportedOperationException("商品批量导入v2模板下载将在后续任务实现");
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
        String categoryId = first.categoryPath.contains("-")
                ? first.categoryPath.substring(0, first.categoryPath.indexOf('-')) : first.categoryPath;
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
     * 校验非负整数字段（库存必填、预警库存选填默认 0）。
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
        if (options == null) {
            return false;
        }
        for (T option : options) {
            String id = idGetter.apply(option);
            String name = nameGetter.apply(option);
            if (value.equals(id) || value.equals(name) || (id != null && name != null && value.equals(id + "-" + name))) {
                return true;
            }
        }
        return false;
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
