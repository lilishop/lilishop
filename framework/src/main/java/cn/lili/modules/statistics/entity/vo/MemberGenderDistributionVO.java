package cn.lili.modules.statistics.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 会员性别分布数据
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
@Data
public class MemberGenderDistributionVO {
    @Schema(description = "性别：1男0女")
    private Integer sex;
    @Schema(description = "客户数")
    private Long num = 0L;
    @Schema(description = "占比(%)")
    private Double proportion = 0D;
}
