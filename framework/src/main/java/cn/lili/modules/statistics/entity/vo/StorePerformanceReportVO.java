package cn.lili.modules.statistics.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 店铺业绩报表
 *
 * @author Bulbasaur
 * @since 2026/07/20
 */
@Data
@Schema(description = "店铺业绩报表")
public class StorePerformanceReportVO {

    @Schema(description = "统计日期(按日)")
    private String reportDate;

    @Schema(description = "店铺ID")
    private String storeId;

    @Schema(description = "店铺名称")
    private String storeName;

    @Schema(description = "订单支付笔数")
    private Long payOrderCount;

    @Schema(description = "支付金额")
    private Double payAmount;

    @Schema(description = "优惠金额")
    private Double discountAmount;

    @Schema(description = "营业额")
    private Double turnover;

    @Schema(description = "折扣率")
    private Double discountRate;

    @Schema(description = "营业收入")
    private Double operatingIncome;

    @Schema(description = "营业收入占比")
    private Double operatingIncomePercent;

    @Schema(description = "笔单价")
    private Double avgOrderPrice;

    @Schema(description = "退款笔数")
    private Long refundCount;

    @Schema(description = "订单退款金额")
    private Double refundAmount;

    @Schema(description = "下单转化率")
    private String orderConversionRate;

    @Schema(description = "支付转化率")
    private String payConversionRate;

    @Schema(description = "环比差额")
    private Double momDiff;

    @Schema(description = "环比增长率")
    private String momRate;

    @Schema(description = "同比差额")
    private Double yoyDiff;

    @Schema(description = "同比增长率")
    private String yoyRate;
}
