package cn.lili.modules.goods.serviceimpl;

import cn.lili.modules.goods.support.GoodsImportV2TemplateSupport;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.goods.entity.dto.GoodsOperationDTO;
import cn.lili.modules.goods.entity.vo.GoodsImportResultVO;
import cn.lili.modules.goods.service.BrandService;
import cn.lili.modules.goods.service.CategoryBrandService;
import cn.lili.modules.goods.service.CategoryService;
import cn.lili.modules.goods.service.GoodsImportV2Service;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.modules.goods.service.GoodsUnitService;
import cn.lili.modules.goods.service.StoreGoodsLabelService;
import cn.lili.modules.store.service.FreightTemplateService;
import cn.lili.modules.store.service.StoreDetailService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * 商品批量导入v2业务层实现：解析多规格商品 Excel（第 1 行说明、第 2 行表头、第 3 行起数据），
 * 按「商品序号」列将同一商品的多个 SKU 行归并为一组，做序号预检（非法/空缺/复用序号转失败记录）
 * 与组内两级校验（行级 SKU 字段 + 商品级字段，含分类/品牌/运费模板/计量单位/店铺分类的归属校验），
 * 商品级字段按组内继承规则以首行为准；校验通过的组组装为 GoodsOperationDTO 逐个调用
 * goodsService.addGoods 落库（addGoods 自身事务即每商品独立事务，单组失败记 failList 后继续，
 * 实现部分成功）；downloadTemplate 用 XSSF 生成 .xlsx 模板。
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
    static final int COLUMN_COUNT = GoodsImportV2TemplateSupport.COLUMN_COUNT;

    /**
     * 模板第 2 行的 20 列表头文案
     */
    static final String[] TEMPLATE_HEADERS = GoodsImportV2TemplateSupport.TEMPLATE_HEADERS;

    /**
     * 模板第 1 行填写说明
     */
    static final String TEMPLATE_INSTRUCTION = GoodsImportV2TemplateSupport.TEMPLATE_INSTRUCTION;

    /**
     * 数据起始行下标（从 0 开始）
     */
    static final int DATA_START_ROW_INDEX = GoodsImportV2TemplateSupport.DATA_START_ROW_INDEX;

    /**
     * 单次导入数据行上限
     */
    static final int MAX_DATA_ROWS = GoodsImportV2TemplateSupport.MAX_DATA_ROWS;

    /**
     * 单次导入商品数量上限
     */
    static final int MAX_GOODS_COUNT = GoodsImportV2TemplateSupport.MAX_GOODS_COUNT;

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
            GoodsImportResultVO.FailGoods failGoods = GoodsImportV2Parser.buildFailGoods(groupRows, error);
            failGoods.setGoodsNo(goodsNo);
            result.getFailList().add(failGoods);
        }
        result.setFailCount(result.getFailList().size());
        return result;
    }

    @Override
    public void downloadTemplate(HttpServletResponse response) {
        GoodsImportV2TemplateSupport.downloadTemplate(response, currentStoreId(), categoryService,
                storeGoodsLabelService, brandService, freightTemplateService, goodsUnitService, storeDetailService);
    }

    /**
     * 将校验通过（已完成组内继承回填）的商品组组装为 GoodsOperationDTO
     *
     * @param rows 校验通过的商品组数据行
     * @return 商品操作 DTO
     */
    private GoodsOperationDTO buildGoodsOperationDTO(List<Row> rows) {
        return GoodsImportV2DtoBuilder.buildGoodsOperationDTO(rows, categoryService, categoryBrandService,
                freightTemplateService, goodsUnitService, storeGoodsLabelService, currentStoreId());
    }

    /**
     * 解析 Excel 首个 sheet 的数据行
     *
     * @param in Excel 输入流
     * @return 解析的数据行列表
     * @throws IOException 读取失败异常
     */
    static List<Row> parseRows(InputStream in) throws IOException {
        return GoodsImportV2Parser.parseRows(in);
    }

    /**
     * 按商品序号分组
     *
     * @param rows 数据行列表
     * @return 分组映射
     */
    static Map<Integer, List<Row>> groupByGoodsNo(List<Row> rows) {
        return GoodsImportV2Parser.groupByGoodsNo(rows);
    }

    /**
     * 序号预检并分组
     *
     * @param rows 数据行列表
     * @return 预检分组结果
     */
    static GroupingResult splitValidGroups(List<Row> rows) {
        return GoodsImportV2Parser.splitValidGroups(rows);
    }

    /**
     * 校验一个商品组并做组内继承
     *
     * @param goodsNo 商品序号
     * @param rows    该商品组的全部数据行
     * @return null 表示通过，否则返回失败原因
     */
    String validateGroup(Integer goodsNo, List<Row> rows) {
        return GoodsImportV2Validator.validateGroup(goodsNo, rows, categoryService, categoryBrandService,
                freightTemplateService, goodsUnitService, storeGoodsLabelService, currentStoreId());
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
         * C 列：平台分类，格式「分类ID-名称」，商品级字段
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
