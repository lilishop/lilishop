package cn.lili.modules.distribution.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 分销分享链接访问记录DTO
 *
 * @author pikachu
 */
@Data
public class DistributionVisitEventDTO {

    @Schema(description = "客户端事件ID")
    private String eventId;

    @Schema(description = "分享标识")
    private String shareId;

    @Schema(description = "分销员ID")
    private String distributionId;

    @Schema(description = "商品ID")
    private String goodsId;

    @Schema(description = "访客标识")
    private String visitorId;

    @Schema(description = "访客会员ID")
    private String memberId;

    @Schema(description = "访问渠道")
    private String channel;
}
