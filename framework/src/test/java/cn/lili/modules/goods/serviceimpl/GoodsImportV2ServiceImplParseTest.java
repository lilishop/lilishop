package cn.lili.modules.goods.serviceimpl;

import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 商品批量导入v2-Excel行解析与分组单元测试
 *
 * @author Bulbasaur
 * @since 2026-10-20
 */
class GoodsImportV2ServiceImplParseTest {

    /**
     * 模板表头（第 2 行），共 20 列，与导入模板 A-T 列一一对应
     */
    private static final String[] HEADERS = {
            "商品序号", "商品名称", "平台分类", "店铺分类", "品牌", "运费模板", "计量单位", "商品图片",
            "规格1名称", "规格1值", "规格2名称", "规格2值", "规格3名称", "规格3值",
            "SKU编码", "销售价", "成本价", "库存", "重量", "预警库存"
    };

    /**
     * 验证解析跳过说明行/表头/空行，行号为 Excel 实际行号（从 1 开始），数字单元格按显示文本读取。
     */
    @Test
    void parseRowsSkipsBlankRowsAndKeepsExcelRowNo() throws Exception {
        byte[] xlsx = buildWorkbook(false);

        List<GoodsImportV2ServiceImpl.Row> rows = GoodsImportV2ServiceImpl.parseRows(new ByteArrayInputStream(xlsx));

        // 空行（Excel 第 5 行）被跳过，仅保留 3 条数据行
        assertThat(rows).hasSize(3);
        assertThat(rows).extracting(r -> r.excelRowNo).containsExactly(3, 4, 6);

        // 首行商品级字段完整，数字单元格按显示文本读取（不出现 100.0 之类格式）
        GoodsImportV2ServiceImpl.Row first = rows.get(0);
        assertThat(first.goodsNo).isEqualTo("1");
        assertThat(first.goodsName).isEqualTo("测试商品A");
        assertThat(first.spec1Name).isEqualTo("颜色");
        assertThat(first.spec1Value).isEqualTo("红色");
        assertThat(first.price).isEqualTo("9.9");
        assertThat(first.quantity).isEqualTo("100");

        // 序号1 的次行：商品级字段留空，仅保留 SKU 级字段
        GoodsImportV2ServiceImpl.Row second = rows.get(1);
        assertThat(second.goodsNo).isEqualTo("1");
        assertThat(second.goodsName).isEmpty();
        assertThat(second.spec1Name).isEmpty();
        assertThat(second.spec1Value).isEqualTo("蓝色");
    }

    /**
     * 验证按商品序号分组：序号1 两行归为一组，序号2 一行一组，组内保持原始行顺序。
     */
    @Test
    void groupByGoodsNoMergesRowsOfSameGoods() throws Exception {
        byte[] xlsx = buildWorkbook(false);

        List<GoodsImportV2ServiceImpl.Row> rows = GoodsImportV2ServiceImpl.parseRows(new ByteArrayInputStream(xlsx));
        Map<Integer, List<GoodsImportV2ServiceImpl.Row>> groups = GoodsImportV2ServiceImpl.groupByGoodsNo(rows);

        assertThat(groups).hasSize(2);
        assertThat(groups.keySet()).containsExactly(1, 2);
        assertThat(groups.get(1)).hasSize(2);
        assertThat(groups.get(1)).extracting(r -> r.excelRowNo).containsExactly(3, 4);
        assertThat(groups.get(2)).hasSize(1);
        assertThat(groups.get(2).get(0).goodsName).isEqualTo("测试商品B");
    }

    /**
     * 验证次行商品序号留空时沿用上一组（续行归属最近的有效序号）。
     */
    @Test
    void groupByGoodsNoInheritsPreviousGoodsNoWhenBlank() throws Exception {
        byte[] xlsx = buildWorkbook(true);

        List<GoodsImportV2ServiceImpl.Row> rows = GoodsImportV2ServiceImpl.parseRows(new ByteArrayInputStream(xlsx));
        Map<Integer, List<GoodsImportV2ServiceImpl.Row>> groups = GoodsImportV2ServiceImpl.groupByGoodsNo(rows);

        assertThat(groups).hasSize(2);
        assertThat(groups.get(1)).hasSize(2);
        assertThat(groups.get(2)).hasSize(1);
    }

    /**
     * 构造内存 xlsx：第 1 行说明、第 2 行表头、第 3/4 行为序号1 的两个 SKU（次行商品级字段留空）、
     * 第 5 行为整行空白、第 6 行为序号2 的单规格商品。
     *
     * @param blankGoodsNoOnContinuation 为 true 时序号1 次行的商品序号也留空，验证续行继承
     * @return xlsx 字节数组
     */
    private byte[] buildWorkbook(boolean blankGoodsNoOnContinuation) throws Exception {
        try (Workbook workbook = new XSSFWorkbook(); ByteArrayOutputStream out = new ByteArrayOutputStream()) {
            Sheet sheet = workbook.createSheet("商品导入");
            // 第 1 行：填写说明
            sheet.createRow(0).createCell(0).setCellValue("填写说明：第2行为表头，第3行起填写数据，商品级字段只在同组首行填写");
            // 第 2 行：表头
            Row header = sheet.createRow(1);
            for (int i = 0; i < HEADERS.length; i++) {
                header.createCell(i).setCellValue(HEADERS[i]);
            }
            // 第 3 行：序号1 首行（商品级字段完整）
            Row first = sheet.createRow(2);
            setCell(first, 0, "1");
            setCell(first, 1, "测试商品A");
            setCell(first, 2, "一级/二级/三级");
            setCell(first, 5, "默认运费模板");
            setCell(first, 6, "件");
            setCell(first, 7, "https://img.example.com/a.jpg");
            setCell(first, 8, "颜色");
            setCell(first, 9, "红色");
            first.createCell(15).setCellValue(9.9);
            first.createCell(16).setCellValue(5.5);
            first.createCell(17).setCellValue(100);
            first.createCell(18).setCellValue(0.5);
            first.createCell(19).setCellValue(10);
            // 第 4 行：序号1 次行（商品级字段留空，仅 SKU 级字段）
            Row second = sheet.createRow(3);
            if (!blankGoodsNoOnContinuation) {
                setCell(second, 0, "1");
            }
            setCell(second, 9, "蓝色");
            second.createCell(15).setCellValue(12.9);
            second.createCell(16).setCellValue(6.5);
            second.createCell(17).setCellValue(50);
            // 第 5 行：整行空白
            sheet.createRow(4).createCell(0).setCellValue("");
            // 第 6 行：序号2 单规格商品
            Row third = sheet.createRow(5);
            setCell(third, 0, "2");
            setCell(third, 1, "测试商品B");
            setCell(third, 2, "一级/二级/三级");
            setCell(third, 5, "默认运费模板");
            setCell(third, 6, "件");
            setCell(third, 7, "https://img.example.com/b.jpg");
            third.createCell(15).setCellValue(19.9);
            third.createCell(16).setCellValue(9.5);
            third.createCell(17).setCellValue(200);
            workbook.write(out);
            return out.toByteArray();
        }
    }

    /**
     * 写入字符串单元格。
     */
    private void setCell(Row row, int col, String value) {
        row.createCell(col).setCellValue(value);
    }
}
