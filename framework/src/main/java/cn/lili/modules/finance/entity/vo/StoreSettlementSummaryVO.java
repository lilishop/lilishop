package cn.lili.modules.finance.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 店铺结算汇总
 */
@Data
@Schema(description = "店铺结算汇总")
public class StoreSettlementSummaryVO {

    @Schema(description = "店铺ID")
    private String storeId;

    @Schema(description = "店铺名称")
    private String storeName;

    @Schema(description = "订单实付合计")
    private Double orderPrice;

    @Schema(description = "退款合计")
    private Double refundPrice;

    @Schema(description = "平台佣金")
    private Double commissionPrice;

    @Schema(description = "分销支出")
    private Double distributionCommission;

    @Schema(description = "平台券补贴")
    private Double siteCouponCommission;

    @Schema(description = "礼品卡补贴")
    private Double giftCardSubsidy;

    @Schema(description = "商家应结合计")
    private Double billPrice;

    @Schema(description = "已出账金额")
    private Double outBillAmount;

    @Schema(description = "已对账金额")
    private Double checkBillAmount;

    @Schema(description = "已付款金额")
    private Double completeBillAmount;
}
