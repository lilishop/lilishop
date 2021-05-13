package cn.lili.modules.statistics.model.vo;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 订单统计概述
 *
 * @author Chopper
 * @date 2021-03-03 10:27
 */
@Data
public class OrderOverviewVO {

    @ApiModelProperty(value = "UV人次")
    private Integer uvNum;

    //下单统计
    @ApiModelProperty(value = "下单数量")
    private Long orderNum;

    @ApiModelProperty(value = "下单人数")
    private Long orderMemberNum;

    @ApiModelProperty(value = "下单金额")
    private Double orderAmount;

    //付款统计
    @ApiModelProperty(value = "付款订单数量")
    private Long paymentOrderNum;

    @ApiModelProperty(value = "付款人数")
    private Long paymentsNum;

    @ApiModelProperty(value = "付款金额")
    private Double paymentAmount;


    //退单统计
    @ApiModelProperty(value = "退单笔数")
    private Long refundOrderNum;

    @ApiModelProperty(value = "退单金额")
    private Double refundOrderPrice;

    // 转换率
    @ApiModelProperty(value = "下单转换率")
    private String orderConversionRate;

    @ApiModelProperty(value = "付款转换率")
    private String paymentsConversionRate;

    @ApiModelProperty(value = "整体转换率")
    private String overallConversionRate;


}
