package cn.lili.modules.order.aftersale.entity.vo;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 售后数量VO
 *
 * @author Bulbasaur
 * @since 2021/3/12 10:32 上午
 */
@Data
public class AfterSaleNumVO {

    @ApiModelProperty(value = "申请中售后数量")
    private Integer applyNum;
    @ApiModelProperty(value = "已通过售后数量")
    private Integer passNum;
    @ApiModelProperty(value = "已拒绝售后数量")
    private Integer refuseNum;
    @ApiModelProperty(value = "待卖家收货售后数量")
    private Integer buyerReturnNum;
    @ApiModelProperty(value = "卖家确认收货售后数量")
    private Integer sellerConfirmNum;
    @ApiModelProperty(value = "卖家终止售后售后数量")
    private Integer sellerTerminationNum;
    @ApiModelProperty(value = "买家取消售后售后数量")
    private Integer buyerCancelNum;
    @ApiModelProperty(value = "等待平台退款售后数量")
    private Integer waitRefundNum;
    @ApiModelProperty(value = "已完成售后数量")
    private Integer completeNum;
}
