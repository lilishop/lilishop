package cn.lili.modules.goods.serviceimpl;

import cn.lili.modules.goods.entity.vo.GoodsImportResultVO;
import cn.lili.modules.goods.service.GoodsImportV2Service;
import org.apache.poi.ss.usermodel.DataFormatter;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * 商品批量导入v2业务层实现：解析多规格商品 Excel（第 1 行说明、第 2 行表头、第 3 行起数据），
 * 按「商品序号」列将同一商品的多个 SKU 行归并为一组，校验与落库在后续任务叠加。
 * 本类当前实现行解析与分组，importGoods/downloadTemplate 主流程待后续任务补全。
 *
 * @author Bulbasaur
 * @since 2026-10-20
 */
@Service
public class GoodsImportV2ServiceImpl implements GoodsImportV2Service {

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
     * 导入数据行，保存 20 列原始字符串（A-T 列）及 Excel 实际行号（从 1 开始）
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
        final String goodsName;

        /**
         * C 列：平台分类路径，格式「一级/二级/三级」，商品级字段
         */
        final String categoryPath;

        /**
         * D 列：店铺分类，多个用英文逗号分隔，商品级字段
         */
        final String storeCategory;

        /**
         * E 列：品牌，商品级字段
         */
        final String brand;

        /**
         * F 列：运费模板，商品级字段
         */
        final String freightTemplate;

        /**
         * G 列：计量单位，商品级字段
         */
        final String goodsUnit;

        /**
         * H 列：商品图片 URL，多个英文逗号分隔，首张为主图，商品级字段
         */
        final String goodsImages;

        /**
         * I 列：规格1名称，商品级字段，只在同组首行填写
         */
        final String spec1Name;

        /**
         * J 列：规格1值，SKU 级字段
         */
        final String spec1Value;

        /**
         * K 列：规格2名称，商品级字段，只在同组首行填写
         */
        final String spec2Name;

        /**
         * L 列：规格2值，SKU 级字段
         */
        final String spec2Value;

        /**
         * M 列：规格3名称，商品级字段，只在同组首行填写
         */
        final String spec3Name;

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
