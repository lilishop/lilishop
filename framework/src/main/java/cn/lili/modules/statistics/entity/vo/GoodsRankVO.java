package cn.lili.modules.statistics.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 商品SKU排行数据
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
@Data
public class GoodsRankVO {

    @Schema(description = "排名")
    private Integer rank;
    @Schema(description = "SKU ID")
    private String skuId;
    @Schema(description = "商品SKU名称")
    private String skuName;
    @Schema(description = "金额(退款金额或净销售额)")
    private Double amount;
}
