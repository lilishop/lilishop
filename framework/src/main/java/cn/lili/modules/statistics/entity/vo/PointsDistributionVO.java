package cn.lili.modules.statistics.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 客户可用积分分布数据
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
@Data
public class PointsDistributionVO {
    @Schema(description = "积分值区间")
    private String pointRange;
    @Schema(description = "客户数")
    private Long memberNum = 0L;
    @Schema(description = "占比(%)")
    private Double proportion = 0D;
}
