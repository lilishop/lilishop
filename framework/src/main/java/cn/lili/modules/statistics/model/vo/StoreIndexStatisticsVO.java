package cn.lili.modules.statistics.model.vo;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 店铺首页数据
 *
 * @author Chopper
 * @date 2021/3/17 4:04 下午
 */
@Data
public class StoreIndexStatisticsVO {

    @ApiModelProperty(value = "商品总数量")
    private Integer goodsNum;
    @ApiModelProperty(value = "订单总数量")
    private Integer orderNum;
    @ApiModelProperty(value = "订单总额")
    private Double orderPrice;
    @ApiModelProperty(value = "访客数UV")
    private Integer storeUV;

    @ApiModelProperty(value = "待付款订单数量")
    private Integer unPaidOrder;
    @ApiModelProperty(value = "待发货订单数量")
    private Integer unDeliveredOrder;
    @ApiModelProperty(value = "待收货订单数量")
    private Integer deliveredOrder;

    @ApiModelProperty(value = "待处理退货数量")
    private Integer returnGoods;
    @ApiModelProperty(value = "待处理退款数量")
    private Integer returnMoney;
    @ApiModelProperty(value = "待回复评价数量")
    private Integer memberEvaluation;
    @ApiModelProperty(value = "待处理交易投诉数量")
    private Integer complaint;

    @ApiModelProperty(value = "待上架商品数量")
    private Integer waitUpper;
    @ApiModelProperty(value = "待审核商品数量")
    private Integer waitAuth;

    @ApiModelProperty(value = "可参与秒杀活动数量")
    private Integer seckillNum;
    @ApiModelProperty(value = "未对账结算单数量")
    private Integer waitPayBill;


}
