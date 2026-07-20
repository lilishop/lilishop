package cn.lili.modules.statistics.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 客户身份积分累计统计数据
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
@Data
public class PointsIdentityStatVO {
    @Schema(description = "客户身份/等级名称")
    private String identity;
    @Schema(description = "累计发送积分")
    private Long totalIssued = 0L;
    @Schema(description = "累计消耗积分")
    private Long usedPoint = 0L;
    @Schema(description = "可用积分")
    private Long available = 0L;
    @Schema(description = "可用积分占比(%)")
    private Double proportion = 0D;
    @Schema(description = "人均可用积分")
    private Double avgAvailable = 0D;
}
