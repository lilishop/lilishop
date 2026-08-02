package cn.lili.modules.goods.serviceimpl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.convert.Convert;
import cn.hutool.poi.excel.ExcelReader;
import cn.hutool.poi.excel.ExcelUtil;
import cn.hutool.poi.excel.ExcelWriter;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 验证导入模板表头与 parseImportFile（第 1 行表头、第 2 行起数据）一致。
 *
 * @author Mike
 * @date 2026-07-31
 */
class CardKeyImportTemplateTest {

    @Test
    void templateWithRow2DataParsesFromSecondRow() throws Exception {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        ExcelWriter writer = ExcelUtil.getWriter(true);
        writer.writeHeadRow(CollUtil.newArrayList("卡号", "卡密"));
        writer.writeRow(CollUtil.newArrayList("GAME-2026-0001", "Xk9mP2nQ8rTv"));
        writer.flush(out, true);
        writer.close();

        try (ExcelReader reader = ExcelUtil.getReader(new ByteArrayInputStream(out.toByteArray()))) {
            List<List<Object>> header = reader.read(0, 0);
            assertThat(Convert.toStr(header.get(0).get(0))).isEqualTo("卡号");
            assertThat(Convert.toStr(header.get(0).get(1))).isEqualTo("卡密");

            List<List<Object>> data = reader.read(1, reader.getRowCount() - 1);
            assertThat(data).hasSize(1);
            assertThat(Convert.toStr(data.get(0).get(0))).isEqualTo("GAME-2026-0001");
            assertThat(Convert.toStr(data.get(0).get(1))).isEqualTo("Xk9mP2nQ8rTv");
        }
    }

    @Test
    void headerOnlyTemplateProducesNoDataRows() throws Exception {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        ExcelWriter writer = ExcelUtil.getWriter(true);
        writer.writeHeadRow(CollUtil.newArrayList("卡号", "卡密"));
        writer.flush(out, true);
        writer.close();

        try (ExcelReader reader = ExcelUtil.getReader(new ByteArrayInputStream(out.toByteArray()))) {
            List<List<Object>> data = reader.read(1, reader.getRowCount() - 1);
            assertThat(data).isEmpty();
        }
    }
}
