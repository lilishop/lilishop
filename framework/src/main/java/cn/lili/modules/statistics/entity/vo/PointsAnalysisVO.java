package cn.lili.modules.statistics.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 积分分析概览数据
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
@Data
public class PointsAnalysisVO {
    @Schema(description = "累积发放积分")
    private Long totalIssued = 0L;
    @Schema(description = "可用积分")
    private Long availablePoint = 0L;
    @Schema(description = "累积消耗积分")
    private Long usedPoint = 0L;
    @Schema(description = "积分消耗率(%)")
    private Double usedRate = 0D;
}
