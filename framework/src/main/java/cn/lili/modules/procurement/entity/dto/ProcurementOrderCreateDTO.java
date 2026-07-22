package cn.lili.modules.procurement.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.util.List;

/**
 * 采购单创建请求对象
 * 承载采购单创建时的备注与商品明细
 * @author Bulbasaur
 * @since 2025-12-18
 */
@Data
public class ProcurementOrderCreateDTO {

    @Schema(description = "备注")
    private String remark;

    @Schema(description = "出入库原因ID")
    private String stockReasonId;

    @Schema(description = "商品明细")
    private List<Item> items;

    @Data
    public static class Item {
        private String goodsId;
        private String skuId;
        private String goodsName;
        private Double retailPrice;
        private Integer quantity;
        private Integer taxRate;
        private Double unitPriceWithTax;
    }
}
