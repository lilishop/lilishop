package cn.lili.modules.finance.export;

import cn.hutool.core.io.IoUtil;
import cn.hutool.poi.excel.ExcelUtil;
import cn.hutool.poi.excel.ExcelWriter;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.function.Consumer;

/**
 * 财务导出 Excel 工具类。
 * <p>
 * 封装 Hutool POI 的 xlsx 写出、响应头设置及导出行数校验，
 * 供 {@link cn.lili.modules.finance.serviceimpl.FinanceExportServiceImpl}
 * 和 {@link cn.lili.modules.finance.serviceimpl.FinanceReportServiceImpl} 复用。
 */
@Slf4j
public final class FinanceExportHelper {

    /** 单次导出最大行数，超出时抛出业务异常提示缩小筛选范围 */
    public static final int MAX_EXPORT_ROWS = 50_000;

    private FinanceExportHelper() {
    }

    /**
     * 校验导出行数是否超过上限。
     *
     * @param count 待导出行数
     * @throws ServiceException 超过 {@link #MAX_EXPORT_ROWS} 时
     */
    public static void checkRowLimit(long count) {
        if (count > MAX_EXPORT_ROWS) {
            throw new ServiceException(ResultCode.PARAMS_ERROR,
                    "导出数据超过上限 " + MAX_EXPORT_ROWS + " 行，请缩小筛选范围");
        }
    }

    /**
     * 将 Excel 内容写入 HTTP 响应，输出格式统一为 xlsx。
     *
     * @param response       HTTP 响应
     * @param fileName       下载文件名（不含扩展名，自动追加 .xlsx）
     * @param writerConsumer 向 {@link ExcelWriter} 写入 Sheet 内容的回调
     */
    public static void writeExcel(HttpServletResponse response, String fileName,
                                  Consumer<ExcelWriter> writerConsumer) {
        ExcelWriter writer = ExcelUtil.getWriter(true);
        ServletOutputStream out = null;
        try {
            writerConsumer.accept(writer);
            response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;charset=utf-8");
            response.setHeader("Content-Disposition",
                    "attachment;filename=" + URLEncoder.encode(fileName, StandardCharsets.UTF_8) + ".xlsx");
            out = response.getOutputStream();
            writer.flush(out, true);
        } catch (ServiceException e) {
            throw e;
        } catch (Exception e) {
            log.error("财务导出失败: {}", fileName, e);
            throw new ServiceException(ResultCode.ERROR);
        } finally {
            writer.close();
            IoUtil.close(out);
        }
    }

    /**
     * 切换到指定 Sheet。单 Sheet 导出时重命名默认 sheet1，避免留下空白页。
     */
    public static void useSheet(ExcelWriter writer, String sheetName) {
        if (writer.getWorkbook().getNumberOfSheets() == 1) {
            writer.renameSheet(0, sheetName);
            writer.setSheet(0);
        } else {
            writer.setSheet(sheetName);
        }
    }

    /**
     * 向指定 Sheet 写入表头与数据行。
     */
    public static void writeSheet(ExcelWriter writer, String sheetName,
                                  List<String> headers, List<List<Object>> rows) {
        useSheet(writer, sheetName);
        writer.writeHeadRow(headers);
        if (rows != null && !rows.isEmpty()) {
            writer.write(rows, false);
        }
    }
}
