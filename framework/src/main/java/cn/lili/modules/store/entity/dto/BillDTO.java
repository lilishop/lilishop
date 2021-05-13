package cn.lili.modules.store.entity.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.persistence.Column;
import java.io.Serializable;

/**
 * 结算单传输对象
 *
 * @author Chopper
 * @date 2020/11/17 4:26 下午
 */
@Data
public class BillDTO implements Serializable {

    private static final long serialVersionUID = 4441580387361184989L;


    @Column(name = "order_price")
    @ApiModelProperty(value = "结算周期内订单付款总金额")
    private Double orderPrice;

    @Column(name = "refund_price")
    @ApiModelProperty(value = "退单金额")
    private Double refundPrice;

    @Column(name = "commission_price")
    @ApiModelProperty(value = "平台收取佣金")
    private Double commissionPrice;

    @Column(name = "refund_commission_price")
    @ApiModelProperty(value = "退单产生退还佣金金额")
    private Double refundCommissionPrice;

    @Column(name = "distribution_commission")
    @ApiModelProperty(value = "分销返现支出")
    private Double distributionCommission;

    @Column(name = "distribution_refund_commission")
    @ApiModelProperty(value = "分销订单退还，返现佣金返还")
    private Double distributionRefundCommission;

    @Column(name = "site_coupon_commission")
    @ApiModelProperty(value = "平台优惠券补贴")
    private Double siteCouponCommission;

    @Column(name = "site_coupon_refund_commission")
    @ApiModelProperty(value = "退货平台优惠券补贴返还")
    private Double siteCouponRefundCommission;

    @Column(name = "site_coupon_price")
    @ApiModelProperty(value = "平台优惠券 使用金额")
    private Double siteCouponPrice;
    @Column(name = "site_coupon_point")
    @ApiModelProperty(value = "平台优惠券 返点")
    private Double siteCouponPoint;

}
