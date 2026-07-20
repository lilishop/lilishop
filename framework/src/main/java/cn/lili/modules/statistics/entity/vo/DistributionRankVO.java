package cn.lili.modules.statistics.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 分销排行数据
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
@Data
public class DistributionRankVO {
    @Schema(description = "排名")
    private Integer rank;
    @Schema(description = "ID")
    private String id;
    @Schema(description = "名称")
    private String name;
    @Schema(description = "金额")
    private Double amount = 0D;
    @Schema(description = "数量")
    private Long num = 0L;
}
