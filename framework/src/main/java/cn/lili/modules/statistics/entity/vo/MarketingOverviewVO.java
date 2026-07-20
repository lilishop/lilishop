package cn.lili.modules.statistics.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 营销概况统计数据
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
@Data
public class MarketingOverviewVO {
    @Schema(description = "进行中优惠券数")
    private Long couponActiveNum = 0L;
    @Schema(description = "进行中秒杀数")
    private Long seckillActiveNum = 0L;
    @Schema(description = "进行中拼团数")
    private Long pintuanActiveNum = 0L;
    @Schema(description = "进行中满减数")
    private Long fullDiscountActiveNum = 0L;
    @Schema(description = "进行中砍价商品数")
    private Long kanjiaActiveNum = 0L;
    @Schema(description = "进行中积分商品数")
    private Long pointsGoodsActiveNum = 0L;
    @Schema(description = "优惠券发放量")
    private Long couponPublishNum = 0L;
    @Schema(description = "优惠券领取量")
    private Long couponReceivedNum = 0L;
    @Schema(description = "优惠券核销量")
    private Long couponUsedNum = 0L;
    @Schema(description = "活动优惠总额")
    private Double discountAmount = 0D;
}
