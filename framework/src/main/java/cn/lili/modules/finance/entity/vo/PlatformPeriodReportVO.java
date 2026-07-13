package cn.lili.modules.finance.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 平台经营周期报表行。
 * <p>
 * 数据来源于 {@code li_store_flow} 按周期聚合，商家应结（storeSettlementTotal）
 * 使用 bill_price 净额，与结算单公式一致。
 */
@Data
@Schema(description = "平台经营周期报表")
public class PlatformPeriodReportVO {

    @Schema(description = "报表日期/月份")
    private String period;

    @Schema(description = "成交GMV（支付流水）")
    private Double gmv;

    @Schema(description = "退款金额")
    private Double refundAmount;

    @Schema(description = "净GMV")
    private Double netGmv;

    @Schema(description = "平台佣金收入")
    private Double commissionIncome;

    @Schema(description = "退佣返还")
    private Double refundCommission;

    @Schema(description = "平台优惠券补贴")
    private Double couponSubsidy;

    @Schema(description = "退券补贴返还")
    private Double refundCouponSubsidy;

    @Schema(description = "礼品卡补贴")
    private Double giftCardSubsidy;

    @Schema(description = "退礼品卡补贴")
    private Double refundGiftCardSubsidy;

    @Schema(description = "积分补贴")
    private Double pointSubsidy;

    @Schema(description = "退积分补贴")
    private Double refundPointSubsidy;

    @Schema(description = "砍价补贴")
    private Double kanjiaSubsidy;

    @Schema(description = "退砍价补贴")
    private Double refundKanjiaSubsidy;

    @Schema(description = "限时直降补贴")
    private Double flashDiscountSubsidy;

    @Schema(description = "退限时直降补贴")
    private Double refundFlashDiscountSubsidy;

    @Schema(description = "第N件优惠补贴")
    private Double nthItemSubsidy;

    @Schema(description = "退第N件优惠补贴")
    private Double refundNthItemSubsidy;

    @Schema(description = "分销支出")
    private Double distributionExpense;

    @Schema(description = "退分销返还")
    private Double refundDistribution;

    @Schema(description = "商家应结合计")
    private Double storeSettlementTotal;
}
