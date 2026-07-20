package cn.lili.modules.statistics.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 会员概况统计数据
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
@Data
public class MemberOverviewVO {

    @Schema(description = "累积会员数")
    private OverViewMetricVO totalMemberNum;
    @Schema(description = "新增会员数")
    private OverViewMetricVO newMemberNum;
    @Schema(description = "支付会员数")
    private OverViewMetricVO payMemberNum;
    @Schema(description = "储值会员数")
    private OverViewMetricVO rechargeMemberNum;
    @Schema(description = "会员支付金额")
    private OverViewMetricVO payAmount;
    @Schema(description = "会员支付订单数")
    private OverViewMetricVO payOrderNum;
    @Schema(description = "会员客单价")
    private OverViewMetricVO customerPrice;
}
