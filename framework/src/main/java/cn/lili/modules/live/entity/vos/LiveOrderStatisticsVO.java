package cn.lili.modules.live.entity.vos;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 订单统计VO
 * @author chc
 * @since 2022/6/2114:46
 */
@Data
@NoArgsConstructor
@Schema(title = "直播订单统计VO")
public class LiveOrderStatisticsVO {

    @Schema(title = "成交金额")
    private Double totalAmount;

    @Schema(title = "销量")
    private Integer salesVolume;

    @Schema(title = "成交人数")
    private Long dealUserCount;

    @Schema(title = "客单价")
    private Double averageOrderValue;

    @Schema(title = "转化率")
    private Double conversionRate;
}
