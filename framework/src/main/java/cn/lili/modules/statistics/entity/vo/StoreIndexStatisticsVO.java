package cn.lili.modules.statistics.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 店铺首页数据
 *
 * @author Chopper
 * @since 2021/3/17 4:04 下午
 */
@Data
public class StoreIndexStatisticsVO {

    @Schema(description = "商品总数量")
    private Long goodsNum;
    @Schema(description = "订单总数量")
    private Integer orderNum;
    @Schema(description = "订单总额")
    private Double orderPrice;
    @Schema(description = "访客数UV")
    private Integer storeUV;

    @Schema(description = "待付款订单数量")
    private Long unPaidOrder;
    @Schema(description = "待发货订单数量")
    private Long unDeliveredOrder;
    @Schema(description = "待收货订单数量")
    private Long deliveredOrder;
    @Schema(description = "待核验订单数量")
    private Long waitCheckOrder;

    @Schema(description = "待处理退货数量")
    private Long returnGoods;
    @Schema(description = "待处理退款数量")
    private Long returnMoney;
    @Schema(description = "待回复评价数量")
    private Long memberEvaluation;
    @Schema(description = "待处理交易投诉数量")
    private Long complaint;

    @Schema(description = "待上架商品数量")
    private Long waitUpper;
    @Schema(description = "待审核商品数量")
    private Long waitAuth;

    @Schema(description = "可参与秒杀活动数量")
    private Long seckillNum;
    @Schema(description = "未对账结算单数量")
    private Long waitPayBill;

    @Schema(description = "待自提数量")
    private Long selfPickNum;

    @Schema(description = "警告库存")
    private Long alertQuantityNum;


}
