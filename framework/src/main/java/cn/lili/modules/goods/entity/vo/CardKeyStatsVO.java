package cn.lili.modules.goods.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 卡池状态统计
 *
 * @author Mike
 * @date 2026-07-31
 */
@Data
@Schema(description = "卡池状态统计")
public class CardKeyStatsVO implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "SKU ID")
    private String skuId;

    @Schema(description = "在库可售")
    private Integer unusedCount;

    @Schema(description = "预占（拼团等）")
    private Integer reservedCount;

    @Schema(description = "已售")
    private Integer allocatedCount;

    @Schema(description = "作废")
    private Integer voidedCount;
}
