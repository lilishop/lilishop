package cn.lili.modules.procurement.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Data
/**
 * 报损单创建的明细项请求对象
 * @author Bulbasaur
 * @since 2025-12-18
 */
public class DamageReportItemDTO {
    @Schema(description = "商品ID")
    private String goodsId;
    @Schema(description = "SKU ID")
    private String skuId;
    @Schema(description = "商品名称")
    private String goodsName;
    @Schema(description = "报损数量")
    private Integer quantity;
    @Schema(description = "单价")
    private Double unitPrice;
}
