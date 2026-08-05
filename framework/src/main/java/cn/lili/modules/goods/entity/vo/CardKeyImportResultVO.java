package cn.lili.modules.goods.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * 卡密导入结果
 *
 * @author Mike
 * @date 2026-07-31
 */
@Data
@Schema(description = "卡密导入结果")
public class CardKeyImportResultVO implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "成功条数")
    private Integer successCount = 0;

    @Schema(description = "失败条数")
    private Integer failCount = 0;

    @Schema(description = "失败明细")
    private List<FailRow> failRows = new ArrayList<>();

    @Data
    @Schema(description = "导入失败行")
    public static class FailRow implements Serializable {

        private static final long serialVersionUID = 1L;

        @Schema(description = "Excel 行号")
        private Integer row;

        @Schema(description = "卡号")
        private String cardNo;

        @Schema(description = "失败原因")
        private String reason;
    }
}
