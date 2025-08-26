package cn.lili.modules.statistics.entity.vo;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 概览数据
 * @author Bulbasaur
 * @since 2025/08/25 7:07 下午
 */
@Data
public class OverViewDataVO {


    @ApiModelProperty(value = "营业额")
    private Double turnover;
    @ApiModelProperty(value = "优惠金额")
    private Double discount;
    @ApiModelProperty(value = "营业收入不含数值储值金额")
    private Double incomeNoStoreValue;
    @ApiModelProperty(value = "支付订单数")
    private Long payOrderNum;
    @ApiModelProperty(value = "新增充值金额")
    private Double recharge;
    @ApiModelProperty(value = "使用充值金额")
    private Long rechargeUse;




}
