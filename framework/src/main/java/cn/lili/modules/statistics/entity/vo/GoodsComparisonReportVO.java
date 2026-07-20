package cn.lili.modules.statistics.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 商品同比环比报表
 *
 * @author Bulbasaur
 * @since 2026/07/20
 */
@Data
@Schema(description = "商品同比环比报表")
public class GoodsComparisonReportVO {

    @Schema(description = "商品ID")
    private String goodsId;

    @Schema(description = "商品名称")
    private String goodsName;

    @Schema(description = "统计时间")
    private String reportTime;

    @Schema(description = "店铺ID")
    private String storeId;

    @Schema(description = "店铺名称")
    private String storeName;

    @Schema(description = "商品销售金额")
    private Double salesAmount;

    @Schema(description = "商品销售总金额")
    private Double totalSalesAmount;

    @Schema(description = "销售金额占比")
    private Double amountPercent;

    @Schema(description = "环比(上期销售额)")
    private Double momAmount;

    @Schema(description = "环比率")
    private String momRate;

    @Schema(description = "环比差额")
    private Double momDiff;

    @Schema(description = "同比(去年同期销售额)")
    private Double yoyAmount;

    @Schema(description = "同比率")
    private String yoyRate;

    @Schema(description = "同比差额")
    private Double yoyDiff;

    @Schema(description = "商品销售数")
    private Long salesNum;

    @Schema(description = "销售数量占比")
    private Double salesNumPercent;
}
