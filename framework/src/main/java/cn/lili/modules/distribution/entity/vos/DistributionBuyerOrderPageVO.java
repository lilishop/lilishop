package cn.lili.modules.distribution.entity.vos;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

/**
 * 分销员推广/邀请订单分页结果
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Data
public class DistributionBuyerOrderPageVO {

    @Schema(description = "总记录数")
    private long total;

    @Schema(description = "当前页")
    private long current;

    @Schema(description = "每页数量")
    private long size;

    @Schema(description = "佣金合计")
    private BigDecimal totalCommission = BigDecimal.ZERO;

    @Schema(description = "列表数据")
    private List<DistributionBuyerOrderVO> records = new ArrayList<>();
}
