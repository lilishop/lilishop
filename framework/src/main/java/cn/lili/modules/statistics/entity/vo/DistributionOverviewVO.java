package cn.lili.modules.statistics.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 分销概况统计数据
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
@Data
public class DistributionOverviewVO {
    @Schema(description = "分销员数")
    private Long distributorNum = 0L;
    @Schema(description = "待审核分销员数")
    private Long applyNum = 0L;
    @Schema(description = "分销订单数")
    private Long distributionOrderNum = 0L;
    @Schema(description = "分销订单金额")
    private Double distributionOrderAmount = 0D;
    @Schema(description = "已结算佣金")
    private Double settledCommission = 0D;
    @Schema(description = "待结算佣金")
    private Double pendingCommission = 0D;
    @Schema(description = "提现金额")
    private Double cashAmount = 0D;
}
