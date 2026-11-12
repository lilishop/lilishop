package cn.lili.modules.stock.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * 库存流水 MQ 消息体
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "库存流水消息体")
public class StockFlowMessage implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "店铺ID")
    private String storeId;

    @Schema(description = "商品ID")
    private String goodsId;

    @Schema(description = "SKU ID")
    private String skuId;

    @Schema(description = "商品名称")
    private String goodsName;

    @Schema(description = "规格快照")
    private String specs;

    @Schema(description = "变动类型")
    private String flowType;

    @Schema(description = "变动方向")
    private String direction;

    @Schema(description = "变动数量")
    private Integer quantity;

    @Schema(description = "变动前库存")
    private Integer beforeStock;

    @Schema(description = "变动后库存")
    private Integer afterStock;

    @Schema(description = "业务单号")
    private String bizSn;

    @Schema(description = "操作人")
    private String operator;

    @Schema(description = "备注")
    private String remark;
}
