package cn.lili.modules.system.entity.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;

/**
 * 订单设置
 *
 * @author Chopper
 * @since 2020/11/17 7:59 下午
 */
@Data
public class OrderSetting implements Serializable {

    private static final long serialVersionUID = -2628613596000114786L;
    @ApiModelProperty(value = "自动取消 分钟")
    private Integer autoCancel;

    @ApiModelProperty(value = "自动收货 天")
    private Integer autoReceive;

    /**
     * 已完成订单允许退单：X天内，允许客户发起退货退款申请，未发货订单随时可退，未发货订单随时可退
     */
    @ApiModelProperty(value = "已完成订单允许退单 天")
    private Integer closeAfterSale;

    @ApiModelProperty(value = "自动评价 天")
    private Integer autoEvaluation;

    //---------------售后---------------

    @ApiModelProperty(value = "售后自动取消 天")
    private Integer autoCancelAfterSale;

    /**
     * 待审核退单自动审核：X天后，商家逾期未处理的待审核退单，将会自动审核通过。
     */
    @ApiModelProperty(value = "待审核退单自动审核 天")
    private Integer autoAfterSaleReview;

    /**
     * 退单自动确认收货：X天后，商家逾期未处理的待收货退单，将会自动确认收货，非快递退货的退单，再审核通过后开始计时。
     */
    @ApiModelProperty(value = "已完成订单允许退单 天")
    private Integer autoAfterSaleComplete;

    //---------------投诉---------------
    /**
     * 已完成订单允许投诉：X天内，允许客户发起交易投诉
     * 如果写0，则不允许投诉
     */
    @ApiModelProperty(value = "已完成订单允许投诉 天")
    private Integer closeComplaint;
}
