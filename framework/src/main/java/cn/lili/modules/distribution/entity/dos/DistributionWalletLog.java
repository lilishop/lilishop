package cn.lili.modules.distribution.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

/**
 * 分销钱包流水
 *
 * @author pikachu
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Schema(description = "分销钱包流水")
@TableName("li_distribution_wallet_log")
@NoArgsConstructor
public class DistributionWalletLog extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @Schema(description = "分销员ID")
    private String distributionId;

    /**
     * @see cn.lili.modules.distribution.entity.enums.DistributionWalletLogTypeEnum
     */
    @Schema(description = "流水类型")
    private String logType;

    @Schema(description = "业务单号")
    private String bizSn;

    @Schema(description = "变动金额")
    private BigDecimal changeAmount;

    @Schema(description = "变动前可提现余额")
    private BigDecimal canRebateBefore;

    @Schema(description = "变动后可提现余额")
    private BigDecimal canRebateAfter;

    @Schema(description = "变动前佣金冻结余额")
    private BigDecimal commissionFrozenBefore;

    @Schema(description = "变动后佣金冻结余额")
    private BigDecimal commissionFrozenAfter;

    @Schema(description = "变动前提现冻结余额")
    private BigDecimal withdrawalFrozenBefore;

    @Schema(description = "变动后提现冻结余额")
    private BigDecimal withdrawalFrozenAfter;

    @Schema(description = "变动前累计已提现金额")
    private BigDecimal withdrawnTotalBefore;

    @Schema(description = "变动后累计已提现金额")
    private BigDecimal withdrawnTotalAfter;

    @Schema(description = "操作者ID")
    private String operatorId;

    @Schema(description = "备注")
    private String remark;

    @Schema(description = "幂等键")
    private String idempotentKey;
}
