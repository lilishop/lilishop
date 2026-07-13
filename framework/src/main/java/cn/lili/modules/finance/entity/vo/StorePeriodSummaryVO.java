package cn.lili.modules.finance.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 商家端本店周期汇总
 */
@Data
@Schema(description = "店铺周期财务汇总")
public class StorePeriodSummaryVO {

    @Schema(description = "店铺名称")
    private String storeName;

    @Schema(description = "订单实付")
    private Double orderPrice;

    @Schema(description = "退款金额")
    private Double refundPrice;

    @Schema(description = "平台服务费")
    private Double commissionPrice;

    @Schema(description = "分销佣金")
    private Double distributionCommission;

    @Schema(description = "平台券补贴")
    private Double siteCouponCommission;

    @Schema(description = "礼品卡补贴")
    private Double giftCardSubsidy;

    @Schema(description = "应结金额")
    private Double billPrice;
}
