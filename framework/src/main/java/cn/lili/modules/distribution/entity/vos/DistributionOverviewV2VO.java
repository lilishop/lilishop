package cn.lili.modules.distribution.entity.vos;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.math.BigDecimal;

/**
 * 分销概览V2 VO
 *
 * @author pikachu
 */
@Data
public class DistributionOverviewV2VO {

    @Schema(description = "今日新增分销员数")
    private Long todayNewDistributorCount = 0L;

    @Schema(description = "今日新增下级分销员数")
    private Long todayNewSubDistributorCount = 0L;

    @Schema(description = "今日分销员销售额")
    private BigDecimal todaySalesAmount = BigDecimal.ZERO;

    @Schema(description = "今日成交客户数")
    private Long todayPaidCustomerCount = 0L;

    @Schema(description = "今日支出佣金")
    private BigDecimal todayCommissionAmount = BigDecimal.ZERO;

    @Schema(description = "累计分销员数")
    private Long totalDistributorCount = 0L;

    @Schema(description = "累计下级分销员数")
    private Long totalSubDistributorCount = 0L;

    @Schema(description = "累计分销员销售额")
    private BigDecimal totalSalesAmount = BigDecimal.ZERO;

    @Schema(description = "累计成交客户数")
    private Long totalPaidCustomerCount = 0L;

    @Schema(description = "商品分享引流")
    private Long totalShareVisitorCount = 0L;

    @Schema(description = "累计支出佣金")
    private BigDecimal totalCommissionAmount = BigDecimal.ZERO;
}
