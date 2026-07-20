package cn.lili.modules.statistics.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 积分累计分发来源分布数据
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
@Data
public class PointsSourceDistributionVO {
    @Schema(description = "发放途径编码")
    private String source;
    @Schema(description = "发放途径名称")
    private String sourceName;
    @Schema(description = "发送积分值")
    private Long point = 0L;
    @Schema(description = "发送占比(%)")
    private Double proportion = 0D;
}
