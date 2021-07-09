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
    private Integer uvNum = 0;

    /**
     * 下单统计
     */
    @ApiModelProperty(value = "下单数量")
    private Long orderNum = 0L;

    @ApiModelProperty(value = "下单人数")
    private Long orderMemberNum = 0L;

    @ApiModelProperty(value = "下单金额")
    private Double orderAmount = 0D;

    /**
     * 付款统计
     */
    @ApiModelProperty(value = "付款订单数量")
    private Long paymentOrderNum = 0L;

    @ApiModelProperty(value = "付款人数")
    private Long paymentsNum = 0L;

    @ApiModelProperty(value = "付款金额")
    private Double paymentAmount = 0D;


    /**
     * 退单统计
     */
    @ApiModelProperty(value = "退单笔数")
    private Long refundOrderNum = 0L;

    @ApiModelProperty(value = "退单金额")
    private Double refundOrderPrice = 0D;

    /**
     * 转换率
     */
    @ApiModelProperty(value = "下单转换率")
    private String orderConversionRate;

    @ApiModelProperty(value = "付款转换率")
    private String paymentsConversionRate;

    @ApiModelProperty(value = "整体转换率")
    private String overallConversionRate;


}
