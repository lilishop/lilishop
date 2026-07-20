package cn.lili.modules.statistics.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 商品销售汇总报表
 *
 * @author Bulbasaur
 * @since 2026/07/20
 */
@Data
@Schema(description = "商品销售汇总报表")
public class GoodsSalesSummaryReportVO {

    @Schema(description = "商品ID")
    private String goodsId;

    @Schema(description = "销售商品")
    private String goodsName;

    @Schema(description = "商品销售数量")
    private Long salesNum;

    @Schema(description = "商品销售金额")
    private Double salesAmount;

    @Schema(description = "商品退货数量")
    private Long refundNum;

    @Schema(description = "商品退款金额")
    private Double refundAmount;

    @Schema(description = "商品净销售数量")
    private Long netNum;

    @Schema(description = "商品净销售金额")
    private Double netAmount;

    @Schema(description = "占净销售金额百分比")
    private Double netAmountPercent;

    @Schema(description = "商品平均单价")
    private Double avgPrice;

    @Schema(description = "商品售价金额")
    private Double salePriceAmount;

    @Schema(description = "商品优惠金额")
    private Double discountAmount;
}
