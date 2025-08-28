package cn.lili.modules.order.order.entity.vo;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

@Data
public class OrderNumVO {

    @ApiModelProperty(value = "未付款订单数量")
    private Integer waitPayNum;
    @ApiModelProperty(value = "已付款订单数量")
    private Integer waitDeliveryNum;
    @ApiModelProperty(value = "待发货订单数量")
    private Integer waitShipNum;
    @ApiModelProperty(value = "部分发货订单数量")
    private Integer partsDeliveredNumNum;
    @ApiModelProperty(value = "待收货订单数量")
    private Integer deliveredNum;
    @ApiModelProperty(value = "待核验订单数量")
    private Integer waitCheckNum;
    @ApiModelProperty(value = "待自提订单数量")
    private Integer waitSelfPickNum;
    @ApiModelProperty(value = "已完成订单数量")
    private Integer finishNum;
    @ApiModelProperty(value = "已关闭订单数量")
    private Integer closeNum;
}
