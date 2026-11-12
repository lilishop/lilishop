package cn.lili.modules.goods.serviceimpl;

import cn.lili.modules.goods.entity.dos.Category;
import cn.lili.modules.goods.entity.dos.GoodsUnit;
import cn.lili.modules.goods.entity.vos.CategoryBrandVO;
import cn.lili.modules.goods.entity.vos.StoreGoodsLabelVO;
import cn.lili.modules.goods.service.CategoryBrandService;
import cn.lili.modules.goods.service.CategoryService;
import cn.lili.modules.goods.service.GoodsUnitService;
import cn.lili.modules.goods.service.StoreGoodsLabelService;
import cn.lili.modules.store.entity.vos.FreightTemplateVO;
import cn.lili.modules.store.service.FreightTemplateService;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

/**
 * 商品批量导入v2 两级校验支持组件
 * 负责首行商品级字段向续行的组内继承、行级 SKU（价格/成本/库存/重量/预警/规格唯一性）校验，以及商品级字段（名称/图片/分类/品牌/运费模板/单位/店铺分类归属）多级校验。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
public class GoodsImportV2Validator {

    /**
     * 校验一个商品组并做组内继承：先将首行的商品级字段继承到留空的续行，再做行级 SKU 校验与商品级字段校验。
     *
     * @param goodsNo                商品序号（Excel 第 A 列分组标识）
     * @param rows                   该商品组的全部数据行，首行为商品级字段来源
     * @param categoryService        分类服务
     * @param categoryBrandService   分类品牌服务
     * @param freightTemplateService 运费模板服务
     * @param goodsUnitService       计量单位服务
     * @param storeGoodsLabelService 店铺分类服务
     * @param storeId                当前商家店铺 ID
     * @return null 表示校验通过，否则返回含行号的失败原因
     */
    public static String validateGroup(Integer goodsNo,
                                       List<GoodsImportV2ServiceImpl.Row> rows,
                                       CategoryService categoryService,
                                       CategoryBrandService categoryBrandService,
                                       FreightTemplateService freightTemplateService,
                                       GoodsUnitService goodsUnitService,
                                       StoreGoodsLabelService storeGoodsLabelService,
                                       String storeId) {
        if (goodsNo == null) {
            return "商品序号不能为空";
        }
        if (rows == null || rows.isEmpty()) {
            return "商品序号" + goodsNo + "没有数据行";
        }
        GoodsImportV2ServiceImpl.Row first = rows.get(0);
        for (int i = 1; i < rows.size(); i++) {
            inheritProductFields(first, rows.get(i));
        }
        String rowError = validateSkuRows(rows);
        if (rowError != null) {
            return rowError;
        }
        return validateGoodsFields(first, categoryService, categoryBrandService,
                freightTemplateService, goodsUnitService, storeGoodsLabelService, storeId);
    }

    /**
     * 组内继承：将首行商品级字段值填充到续行的留空字段；续行已填写的商品级字段以首行为准，被首行值覆盖。
     *
     * @param first 组内首行（商品级字段来源）
     * @param row   待继承的续行
     */
    public static void inheritProductFields(GoodsImportV2ServiceImpl.Row first, GoodsImportV2ServiceImpl.Row row) {
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
    public static String validateSkuRows(List<GoodsImportV2ServiceImpl.Row> rows) {
        Map<String, Integer> specComboRows = new HashMap<>();
        Map<String, Integer> skuSnRows = new HashMap<>();
        for (GoodsImportV2ServiceImpl.Row row : rows) {
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
            String combo = row.spec1Value + ' ' + row.spec2Value + ' ' + row.spec3Value;
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
     * @param first                  组内首行
     * @param categoryService        分类服务
     * @param categoryBrandService   分类品牌服务
     * @param freightTemplateService 运费模板服务
     * @param goodsUnitService       计量单位服务
     * @param storeGoodsLabelService 店铺分类服务
     * @param storeId                当前商家店铺 ID
     * @return null 表示通过，否则返回含行号的失败原因
     */
    public static String validateGoodsFields(GoodsImportV2ServiceImpl.Row first,
                                            CategoryService categoryService,
                                            CategoryBrandService categoryBrandService,
                                            FreightTemplateService freightTemplateService,
                                            GoodsUnitService goodsUnitService,
                                            StoreGoodsLabelService storeGoodsLabelService,
                                            String storeId) {
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
        String categoryError = checkCategoryPath(categoryId, first.categoryPath, rowNo, categoryService);
        if (categoryError != null) {
            return categoryError;
        }
        if (!first.brand.isEmpty()) {
            List<CategoryBrandVO> brands = categoryBrandService.getCategoryBrandList(categoryId);
            if (!matchesOption(first.brand, brands, CategoryBrandVO::getId, CategoryBrandVO::getName)) {
                return "第" + rowNo + "行：品牌不属于所选分类：" + first.brand;
            }
        }
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
     * @param categoryId      截「-」前的分类 ID
     * @param rawValue        Excel 原始填值，用于失败原因展示
     * @param rowNo           首行行号
     * @param categoryService 分类服务
     * @return null 表示通过，否则返回失败原因
     */
    public static String checkCategoryPath(String categoryId, String rawValue, int rowNo, CategoryService categoryService) {
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
     * 校验金额字段：必填、&gt;0 且最多两位小数。
     *
     * @param value      单元格文本
     * @param fieldLabel 字段中文名
     * @param rowNo      行号
     * @return null 表示通过，否则返回含行号的失败原因
     */
    public static String checkPositiveMoney(String value, String fieldLabel, int rowNo) {
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
     * 校验非负整数字段（库存必填；预警库存选填）。
     *
     * @param value      单元格文本
     * @param fieldLabel 字段中文名
     * @param required   是否必填
     * @param rowNo      行号
     * @return null 表示通过，否则返回含行号的失败原因
     */
    public static String checkNonNegativeInteger(String value, String fieldLabel, boolean required, int rowNo) {
        if (value.isEmpty()) {
            return required ? "第" + rowNo + "行：" + fieldLabel + "不能为空" : null;
        }
        if (!value.matches("\\d+")) {
            return "第" + rowNo + "行：" + fieldLabel + "必须为非负整数";
        }
        return null;
    }

    /**
     * 校验重量字段：必填、数字且 ≥0，单位 kg。
     *
     * @param value 单元格文本
     * @param rowNo 行号
     * @return null 表示通过，否则返回含行号的失败原因
     */
    public static String checkWeight(String value, int rowNo) {
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
     * @param specName  规格名称
     * @param specValue 规格值
     * @param index     规格序号（1-3）
     * @param rowNo     行号
     * @return null 表示通过，否则返回含行号的失败原因
     */
    public static String checkSpecPair(String specName, String specValue, int index, int rowNo) {
        if (!specName.isEmpty() && specValue.isEmpty()) {
            return "第" + rowNo + "行：规格" + index + "值不能为空";
        }
        if (specName.isEmpty() && !specValue.isEmpty()) {
            return "第" + rowNo + "行：规格" + index + "名称未在首行填写，不能填写规格" + index + "值";
        }
        return null;
    }

    /**
     * 判断填值是否命中候选列表中的某一项（支持 ID / 名称 / ID-名称）。
     *
     * @param value      Excel 填值
     * @param options    候选列表
     * @param idGetter   候选项 ID 取值函数
     * @param nameGetter 候选项名称取值函数
     * @param <T>        候选项类型
     * @return 命中返回 true
     */
    public static <T> boolean matchesOption(String value, List<T> options, Function<T, String> idGetter, Function<T, String> nameGetter) {
        return findOption(value, options, idGetter, nameGetter) != null;
    }

    /**
     * 在候选列表中查找填值命中的候选项。
     *
     * @param value      Excel 填值
     * @param options    候选列表
     * @param idGetter   候选项 ID 取值函数
     * @param nameGetter 候选项名称取值函数
     * @param <T>        候选项类型
     * @return 命中的候选项，未命中返回 null
     */
    public static <T> T findOption(String value, List<T> options, Function<T, String> idGetter, Function<T, String> nameGetter) {
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
     * 截取平台分类填值的分类 ID。
     *
     * @param categoryPath 平台分类列原始填值
     * @return 分类 ID
     */
    public static String categoryIdOf(String categoryPath) {
        return categoryPath.contains("-")
                ? categoryPath.substring(0, categoryPath.indexOf('-')) : categoryPath;
    }
}
