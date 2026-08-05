package cn.lili.modules.distribution.entity.vos;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.math.BigDecimal;

/**
 * 买家端业绩统计
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Data
public class DistributionBuyerPerformanceVO {

    @Schema(description = "累计收益（含待结算）")
    private BigDecimal totalEarnings = BigDecimal.ZERO;

    @Schema(description = "待结算收益")
    private BigDecimal pendingEarnings = BigDecimal.ZERO;

    @Schema(description = "已结算收益")
    private BigDecimal settledEarnings = BigDecimal.ZERO;

    @Schema(description = "商品佣金（含待结算）")
    private BigDecimal directCommission = BigDecimal.ZERO;

    @Schema(description = "商品佣金待结算")
    private BigDecimal directPending = BigDecimal.ZERO;

    @Schema(description = "邀请奖励（含待结算）")
    private BigDecimal inviteReward = BigDecimal.ZERO;

    @Schema(description = "邀请奖励待结算")
    private BigDecimal invitePending = BigDecimal.ZERO;

    @Schema(description = "累计销售额")
    private BigDecimal totalSales = BigDecimal.ZERO;

    @Schema(description = "累计推广订单数")
    private Long totalOrders = 0L;

    @Schema(description = "累计客户数")
    private Long totalCustomers = 0L;

    @Schema(description = "累计邀请数")
    private Long totalInvites = 0L;
}
