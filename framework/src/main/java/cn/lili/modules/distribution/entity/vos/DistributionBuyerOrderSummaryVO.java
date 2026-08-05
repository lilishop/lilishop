package cn.lili.modules.distribution.entity.vos;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.math.BigDecimal;

/**
 * 分销员订单列表汇总（总数 + 佣金合计）
 *
 * @author Bulbasaur
 * @since 2026-08-05
 */
@Data
public class DistributionBuyerOrderSummaryVO {

    @Schema(description = "总记录数")
    private long total;

    @Schema(description = "佣金合计")
    private BigDecimal totalCommission;
}
