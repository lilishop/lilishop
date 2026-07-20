package cn.lili.modules.statistics.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 会员地域分布数据
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
@Data
public class MemberRegionDistributionVO {
    @Schema(description = "地域")
    private String region;
    @Schema(description = "客户数")
    private Long num = 0L;
    @Schema(description = "占比(%)")
    private Double proportion = 0D;
}
