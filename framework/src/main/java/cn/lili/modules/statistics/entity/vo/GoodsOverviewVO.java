package cn.lili.modules.statistics.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 商品概况统计数据
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
@Data
public class GoodsOverviewVO {

    @Schema(description = "商品净销售金额")
    private OverViewMetricVO netSalesAmount;
    @Schema(description = "商品售价金额")
    private OverViewMetricVO salePriceAmount;
    @Schema(description = "商品优惠金额")
    private OverViewMetricVO discountAmount;
    @Schema(description = "商品销售金额")
    private OverViewMetricVO salesAmount;
    @Schema(description = "商品退款金额")
    private OverViewMetricVO refundAmount;
    @Schema(description = "商品净销售数量")
    private OverViewMetricVO netSalesNum;
    @Schema(description = "商品销售数量")
    private OverViewMetricVO salesNum;
    @Schema(description = "商品退货数量")
    private OverViewMetricVO refundNum;
}
