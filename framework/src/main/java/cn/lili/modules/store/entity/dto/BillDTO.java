package cn.lili.modules.store.entity.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;

/**
 * 结算单传输对象
 *
 * @author Chopper
 * @since 2020/11/17 4:26 下午
 */
@Data
public class BillDTO implements Serializable {

    private static final long serialVersionUID = 4441580387361184989L;


    @ApiModelProperty(value = "结算周期内订单付款总金额")
    private Double orderPrice;

    @ApiModelProperty(value = "退单金额")
    private Double refundPrice;

    @ApiModelProperty(value = "平台收取佣金")
    private Double commissionPrice;

    @ApiModelProperty(value = "退单产生退还佣金金额")
    private Double refundCommissionPrice;

    @ApiModelProperty(value = "分销返现支出")
    private Double distributionCommission;

    @ApiModelProperty(value = "分销订单退还，返现佣金返还")
    private Double distributionRefundCommission;

    @ApiModelProperty(value = "平台优惠券补贴")
    private Double siteCouponCommission;

    @ApiModelProperty(value = "退货平台优惠券补贴返还")
    private Double siteCouponRefundCommission;

    @ApiModelProperty(value = "平台优惠券 使用金额")
    private Double siteCouponPrice;

    @ApiModelProperty(value = "平台优惠券 返点")
    private Double siteCouponPoint;

}
