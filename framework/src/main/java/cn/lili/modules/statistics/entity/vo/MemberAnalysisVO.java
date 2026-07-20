package cn.lili.modules.statistics.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 客户分析统计数据
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
@Data
public class MemberAnalysisVO {

    @Schema(description = "活跃客户数")
    private Long activeMemberNum = 0L;
    @Schema(description = "复购率(%)")
    private Double repurchaseRate = 0D;
    @Schema(description = "新客占比(%)")
    private Double newCustomerRatio = 0D;
    @Schema(description = "老客占比(%)")
    private Double oldCustomerRatio = 0D;
}
