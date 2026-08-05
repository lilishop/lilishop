package cn.lili.modules.distribution.entity.vos;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

/**
 * 我邀请的下级分销员分页结果
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Data
public class DistributionInvitePageVO {

    @Schema(description = "总记录数")
    private long total;

    @Schema(description = "当前页")
    private long current;

    @Schema(description = "每页数量")
    private long size;

    @Schema(description = "邀请奖励合计")
    private BigDecimal totalInviteReward = BigDecimal.ZERO;

    @Schema(description = "列表数据")
    private List<DistributionInviteeVO> records = new ArrayList<>();
}
