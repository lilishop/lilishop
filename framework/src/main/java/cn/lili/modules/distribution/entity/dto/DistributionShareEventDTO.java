package cn.lili.modules.distribution.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 分销商品分享行为记录DTO
 *
 * @author pikachu
 */
@Data
public class DistributionShareEventDTO {

    @Schema(description = "客户端事件ID")
    private String eventId;

    @Schema(description = "分享标识")
    private String shareId;

    @Schema(description = "分销员ID")
    private String distributionId;

    @Schema(description = "商品ID")
    private String goodsId;

    @Schema(description = "SKU ID")
    private String skuId;

    @Schema(description = "分享渠道")
    private String channel;

    @Schema(description = "触发页面")
    private String triggerPage;

    @Schema(description = "分享按钮类型")
    private String shareButtonType;
}
