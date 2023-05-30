package cn.lili.modules.order.aftersale.entity.dto;


import cn.lili.modules.promotion.entity.dos.BasePromotions;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * 商城退款流水
 *
 * @author Chopper
 * @since 2020/11/17 7:25 下午
 */
@Data
public class AfterSalePriceDetailDTO implements Serializable {


    private static final long serialVersionUID = 8808470688518188146L;
    @ApiModelProperty(value = "商品总金额（商品原价）")
    private Double goodsPrice;

    @ApiModelProperty(value = "配送费")
    private Double freightPrice;

    //============discount price============

    @ApiModelProperty(value = "支付积分")
    private Integer payPoint;

    @ApiModelProperty(value = "优惠金额")
    private Double discountPrice;

    @ApiModelProperty(value = "优惠券金额")
    private Double couponPrice;

    //===========end discount price =============


    //=========distribution==========

    @ApiModelProperty(value = "单品分销返现支出")
    private Double distributionCommission;


    @ApiModelProperty(value = "平台收取交易佣金")
    private Double platFormCommission;

    //=========end distribution==========


    //========= platform coupon==========

    @ApiModelProperty(value = "平台优惠券 使用金额")
    private Double siteCouponPrice;

    @ApiModelProperty(value = "站点优惠券佣金比例")
    private Double siteCouponPoint;

    @ApiModelProperty(value = "站点优惠券佣金")
    private Double siteCouponCommission;
    //=========end platform coupon==========

    @ApiModelProperty(value = "流水金额(入账 出帐金额) = goodsPrice - discountPrice - couponPrice")
    private Double flowPrice;

    @ApiModelProperty(value = "最终结算金额 = flowPrice - platFormCommission - distributionCommission")
    private Double billPrice;

    /**
     * 参与的促销活动
     */
    @ApiModelProperty(value = "参与的促销活动")
    private List<BasePromotions> joinPromotion;


    public AfterSalePriceDetailDTO() {
        goodsPrice = 0d;
        freightPrice = 0d;

        payPoint = 0;
        discountPrice = 0d;

        distributionCommission = 0d;
        platFormCommission = 0d;

        siteCouponPrice = 0d;
        siteCouponPoint = 0d;
        siteCouponCommission = 0d;

        flowPrice = 0d;
        billPrice = 0d;

        joinPromotion = new ArrayList<>();
    }

}
