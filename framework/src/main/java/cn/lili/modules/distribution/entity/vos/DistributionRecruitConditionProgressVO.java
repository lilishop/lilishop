package cn.lili.modules.distribution.entity.vos;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.math.BigDecimal;

/**
 * 分销招募条件进度VO
 *
 * @author pikachu
 */
@Data
public class DistributionRecruitConditionProgressVO {

    @Schema(description = "加入条件类型")
    private String joinConditionType;

    @Schema(description = "自购金额门槛")
    private BigDecimal minSelfPurchaseAmount;

    @Schema(description = "当前自购金额")
    private BigDecimal currentSelfPurchaseAmount;

    @Schema(description = "消费笔数门槛")
    private Integer minConsumeCount;

    @Schema(description = "当前消费笔数")
    private Integer currentConsumeCount;

    @Schema(description = "是否满足条件")
    private Boolean satisfied;

    @Schema(description = "是否已购买指定商品")
    private Boolean purchaseGoodsSatisfied;
}
