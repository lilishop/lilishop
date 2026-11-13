package cn.lili.modules.goods.serviceimpl;

import cn.lili.modules.goods.support.GoodsImportV2TemplateSupport;
import cn.lili.modules.goods.entity.vo.GoodsImportResultVO;
import org.apache.poi.ss.usermodel.DataFormatter;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * 商品批量导入v2 Excel 行解析与分组预检支持组件
 * 负责 POI 单元格安全格式化读取、空行自动过滤、序号非数字/空缺/复用/回退等异常序号预检，将多规格数据行按商品序号归并为独立商品组。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
public class GoodsImportV2Parser {

    /**
     * 解析 Excel 首个 sheet 的数据行：跳过前 2 行（说明+表头），整行空白跳过；
     * 列值用 DataFormatter 取显示文本，避免数字单元格读出科学计数或多余小数。
     *
     * @param in Excel 文件输入流（xlsx/xls 均可，由 WorkbookFactory 识别）
     * @return 数据行列表，按 Excel 行顺序排列
     * @throws IOException 读取文件失败时抛出
     */
    public static List<GoodsImportV2ServiceImpl.Row> parseRows(InputStream in) throws IOException {
        List<GoodsImportV2ServiceImpl.Row> rows = new ArrayList<>();
        DataFormatter formatter = new DataFormatter();
        try (Workbook workbook = WorkbookFactory.create(in)) {
            Sheet sheet = workbook.getSheetAt(0);
            for (int i = GoodsImportV2TemplateSupport.DATA_START_ROW_INDEX; i <= sheet.getLastRowNum(); i++) {
                org.apache.poi.ss.usermodel.Row excelRow = sheet.getRow(i);
                if (excelRow == null) {
                    continue;
                }
                String[] cells = new String[GoodsImportV2TemplateSupport.COLUMN_COUNT];
                boolean blank = true;
                for (int c = 0; c < GoodsImportV2TemplateSupport.COLUMN_COUNT; c++) {
                    cells[c] = formatter.formatCellValue(excelRow.getCell(c)).trim();
                    if (!cells[c].isEmpty()) {
                        blank = false;
                    }
                }
                if (blank) {
                    continue;
                }
                rows.add(new GoodsImportV2ServiceImpl.Row(cells, i + 1));
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
    public static Map<Integer, List<GoodsImportV2ServiceImpl.Row>> groupByGoodsNo(List<GoodsImportV2ServiceImpl.Row> rows) {
        Map<Integer, List<GoodsImportV2ServiceImpl.Row>> groups = new LinkedHashMap<>();
        Integer currentGoodsNo = null;
        for (GoodsImportV2ServiceImpl.Row row : rows) {
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
     * 序号预检并分组：在正式业务校验之前拦截三类非法序号行——
     * ①商品序号非数字；②文件首个有效组之前商品序号留空（无所属组）；③序号复用（同一序号非连续出现）或序号回退。
     * 非法段连同其续行整体转为一条失败记录（失败粒度为商品组），合法组保持原始行顺序。
     *
     * @param rows 解析后的数据行
     * @return 合法分组与序号预检失败记录
     */
    public static GoodsImportV2ServiceImpl.GroupingResult splitValidGroups(List<GoodsImportV2ServiceImpl.Row> rows) {
        GoodsImportV2ServiceImpl.GroupingResult result = new GoodsImportV2ServiceImpl.GroupingResult();
        Set<Integer> seenGoodsNo = new HashSet<>();
        Integer lastGoodsNo = null;
        Integer currentGoodsNo = null;
        List<GoodsImportV2ServiceImpl.Row> invalidRows = null;
        String invalidReason = null;
        for (GoodsImportV2ServiceImpl.Row row : rows) {
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
     * 解析商品序号，非数字时返回 null 而非抛 NumberFormatException。
     *
     * @param goodsNo 商品序号单元格文本
     * @return 数字序号，非法时返回 null
     */
    public static Integer parseGoodsNo(String goodsNo) {
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
    public static GoodsImportResultVO.FailGoods buildFailGoods(List<GoodsImportV2ServiceImpl.Row> rows, String reason) {
        GoodsImportResultVO.FailGoods failGoods = new GoodsImportResultVO.FailGoods();
        int firstRowNo = rows.get(0).excelRowNo;
        int lastRowNo = rows.get(rows.size() - 1).excelRowNo;
        failGoods.setRows(firstRowNo == lastRowNo ? String.valueOf(firstRowNo) : firstRowNo + "-" + lastRowNo);
        failGoods.setReason(reason);
        return failGoods;
    }
}
