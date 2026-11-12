package cn.lili.modules.stock.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

import java.io.Serializable;

/**
 * 商家出库单商品明细数据传输对象
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Data
@Schema(description = "出库单明细参数")
public class StockOutboundItemDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "商品SPU ID")
    private String goodsId;

    @NotBlank(message = "商品SKU不能为空")
    @Schema(description = "商品SKU ID")
    private String skuId;

    @Schema(description = "商品名称")
    private String goodsName;

    @Schema(description = "规格快照")
    private String specs;

    @Schema(description = "计量单位")
    private String goodsUnit;

    @NotNull(message = "出库数量不能为空")
    @Min(value = 1, message = "出库数量必须大于0")
    @Schema(description = "出库数量")
    private Integer quantity;

    @Schema(description = "明细备注")
    private String itemRemark;
}
