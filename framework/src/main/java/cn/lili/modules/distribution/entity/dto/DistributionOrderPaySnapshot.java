package cn.lili.modules.distribution.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.math.BigDecimal;

/**
 * 分销订单支付时分佣依据快照（存于分销订单 JSON）。
 * 与行字段故意重复，保留当时比例与结算模式。
 * 
 * @author Mike
 * @date 2026-09-13
 */
@Data
public class DistributionOrderPaySnapshot {

    @Schema(description = "本行是否分销员自购")
    private Boolean selfPurchase;

    @Schema(description = "自购直接佣金归属模式")
    private String selfPurchaseCommissionMode;

    @Schema(description = "货品ID")
    private String skuId;

    @Schema(description = "商品ID")
    private String goodsId;

    @Schema(description = "单品固定佣金")
    private BigDecimal unitCommission;

    @Schema(description = "购买数量")
    private Integer num;

    @Schema(description = "本明细佣金池金额")
    private BigDecimal commissionPool;

    @Schema(description = "一级商品佣金比例")
    private BigDecimal directRate;

    @Schema(description = "二级邀请佣金比例")
    private BigDecimal inviteRate;

    @Schema(description = "一级分销佣金")
    private BigDecimal directCommission;

    @Schema(description = "二级邀请佣金")
    private BigDecimal inviteCommission;

    @Schema(description = "一级分销员ID")
    private String directDistributionId;

    @Schema(description = "一级分销员等级ID")
    private String directGradeId;

    @Schema(description = "二级邀请分销员ID")
    private String inviteDistributionId;

    @Schema(description = "二级邀请分销员等级ID")
    private String inviteGradeId;

    @Schema(description = "业绩归属分销员ID")
    private String performanceDistributorId;
}
