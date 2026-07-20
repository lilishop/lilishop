package cn.lili.modules.statistics.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 营业概况数据（含环比）
 *
 * @author Bulbasaur
 * @since 2025/08/25 7:07 下午
 */
@Data
public class OverViewDataVO {

    @Schema(description = "营业收入=营业收入不含充值+新增充值金额")
    private OverViewMetricVO income;

    @Schema(description = "营业额=营业收入不含充值+优惠金额")
    private OverViewMetricVO turnover;

    @Schema(description = "优惠金额")
    private OverViewMetricVO discount;

    @Schema(description = "营业收入不含充值金额")
    private OverViewMetricVO incomeNoStoreValue;

    @Schema(description = "支付订单数")
    private OverViewMetricVO payOrderNum;

    @Schema(description = "新增充值金额")
    private OverViewMetricVO recharge;

    @Schema(description = "使用储值本金")
    private OverViewMetricVO rechargeUse;

}
