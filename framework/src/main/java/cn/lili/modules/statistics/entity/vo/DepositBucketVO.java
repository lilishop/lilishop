package cn.lili.modules.statistics.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 储值分析区间分布数据
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
@Data
public class DepositBucketVO {
    @Schema(description = "区间标签")
    private String label;
    @Schema(description = "客户数")
    private Long memberNum = 0L;
}
