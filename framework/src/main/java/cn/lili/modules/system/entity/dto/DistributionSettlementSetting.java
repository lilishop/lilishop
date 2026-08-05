package cn.lili.modules.system.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 分销结算方案设置
 *
 * @author pikachu
 */
@Data
public class DistributionSettlementSetting implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * @see cn.lili.modules.distribution.entity.enums.DistributionSelfPurchaseModeEnum
     */
    @Schema(description = "分销员自购直接佣金归属模式")
    private String selfPurchaseCommissionMode;
}
