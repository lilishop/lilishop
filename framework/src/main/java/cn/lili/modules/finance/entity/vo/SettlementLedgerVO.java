package cn.lili.modules.finance.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 结算台账行。
 * <p>
 * 展示各店铺待结算流水、已出账/已对账未付款及历史已付款累计，用于平台财务对账总览。
 */
@Data
@Schema(description = "结算台账")
public class SettlementLedgerVO {

    @Schema(description = "店铺ID")
    private String storeId;

    @Schema(description = "店铺名称")
    private String storeName;

    @Schema(description = "待结算流水金额")
    private Double pendingFlowAmount;

    @Schema(description = "已出账未付款金额")
    private Double outUnpaidAmount;

    @Schema(description = "已对账未付款金额")
    private Double checkUnpaidAmount;

    @Schema(description = "已付款累计")
    private Double paidAmount;
}
