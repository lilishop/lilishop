package cn.lili.modules.promotion.entity.dto.search;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.math.BigDecimal;

/**
 * 礼品卡活动分页查询条件
 *
 * @author Bulbasaur
 * @since 2026-05-22
 */
@Data
@Schema(description = "礼品卡活动分页查询")
public class GiftCardCashActivitySearchParams {

    @Schema(description = "活动名称（模糊）")
    private String giftCardName;

    @Schema(description = "活动状态：全部/正常/已过期")
    private String activityStatus;

    @Schema(description = "礼品卡面额下限（大于等于）")
    private BigDecimal faceValueMin;

    @Schema(description = "礼品卡面额上限（小于等于）")
    private BigDecimal faceValueMax;

    @Schema(description = "礼品卡库存下限（活动总库存，大于等于）")
    private Integer stockTotalMin;

    @Schema(description = "礼品卡库存上限（活动总库存，小于等于）")
    private Integer stockTotalMax;
}
