package cn.lili.modules.statistics.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 店铺订单/访客统计(店铺业绩报表辅助数据)
 *
 * @author Bulbasaur
 * @since 2026/07/20
 */
@Data
@Schema(description = "店铺订单/访客统计")
public class StoreOrderStatsVO {

    @Schema(description = "店铺ID")
    private String storeId;

    @Schema(description = "下单笔数")
    private Long orderCount;

    @Schema(description = "访客数")
    private Long uvNum;
}
