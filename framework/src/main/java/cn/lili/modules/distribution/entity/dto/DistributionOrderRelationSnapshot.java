package cn.lili.modules.distribution.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 分销订单支付时客户关系快照（存于分销订单 JSON）。
 * 时间字段为已格式化字符串，避免 JSONUtil 将 Date 写成时间戳。
 * 
 * @author Mike
 * @date 2026-09-13
 */
@Data
public class DistributionOrderRelationSnapshot {

    @Schema(description = "客户会员ID")
    private String memberId;

    @Schema(description = "绑定分销员ID")
    private String distributionId;

    @Schema(description = "绑定来源")
    private String bindSource;

    @Schema(description = "锁客模式快照")
    private String bindMode;

    @Schema(description = "永久锁客方式快照")
    private String permanentBindMode;

    @Schema(description = "关系状态")
    private String relationStatus;

    @Schema(description = "绑定时间")
    private String bindTime;

    @Schema(description = "到期时间")
    private String expireAt;

    @Schema(description = "永久生效时间")
    private String permanentAt;

    @Schema(description = "触发转永久的订单号")
    private String triggerOrderSn;
}
