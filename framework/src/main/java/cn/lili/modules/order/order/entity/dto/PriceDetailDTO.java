package cn.lili.modules.order.order.entity.dto;


import cn.lili.common.utils.CurrencyUtil;
import cn.lili.modules.promotion.entity.dto.BasePromotion;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * 商城流水，细节到orderItem
 *
 * @author Chopper
 * @date 2020/11/17 7:25 下午
 */
@Data
public class PriceDetailDTO implements Serializable {

    /**
     * 订单原始总价格
     * 用于订单价格修改金额计算使用
     */
    @ApiModelProperty(value = "订单原始总价格")
    private Double originalPrice;

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

    //========= update price ==========

    @ApiModelProperty(value = "订单修改金额")
    private Double updatePrice;

    //=========end update price==========

    @ApiModelProperty(value = "流水金额(入账 出帐金额) = goodsPrice + freight - discountPrice - couponPrice + updatePrice")
    private Double flowPrice;

    @ApiModelProperty(value = "最终结算金额 = flowPrice - platFormCommission - distributionCommission")
    private Double billPrice;

    /**
     * 参与的促销活动
     */
    @ApiModelProperty(value = "参与的促销活动")
    private List<BasePromotion> joinPromotion;


    public PriceDetailDTO() {
        goodsPrice = 0d;
        freightPrice = 0d;

        payPoint = 0;
        discountPrice = 0d;

        distributionCommission = 0d;
        platFormCommission = 0d;

        siteCouponPrice = 0d;
        siteCouponPoint = 0d;
        siteCouponCommission = 0d;

        updatePrice = 0d;

        flowPrice = 0d;
        billPrice = 0d;

        joinPromotion = new ArrayList<>();
    }

    /**
     * 累加
     *
     * @param priceDetailDTOS
     * @return
     */
    public static PriceDetailDTO accumulationPriceDTO(List<PriceDetailDTO> priceDetailDTOS, PriceDetailDTO originPriceDetail) {


        double goodsPrice = 0d;
        double freightPrice = 0d;

        int payPoint = 0;
        double discountPrice = 0d;

        double distributionCommission = 0d;
        double platFormCommission = 0d;

        double siteCouponPrice = 0d;
        double siteCouponPoint = 0d;
        double siteCouponCommission = 0d;

        double updatePrice = 0d;

        double flowPrice = 0d;
        double billPrice = 0d;

        for (PriceDetailDTO price : priceDetailDTOS) {

            goodsPrice = CurrencyUtil.add(goodsPrice, price.getGoodsPrice());
            freightPrice = CurrencyUtil.add(freightPrice, price.getFreightPrice());
            payPoint = payPoint + price.getPayPoint();
            discountPrice = CurrencyUtil.add(discountPrice, price.getDiscountPrice());

            updatePrice = CurrencyUtil.add(updatePrice, price.getUpdatePrice());


            distributionCommission = CurrencyUtil.add(distributionCommission, price.getDistributionCommission());
            platFormCommission = CurrencyUtil.add(platFormCommission, price.getPlatFormCommission());

            siteCouponPrice = CurrencyUtil.add(siteCouponPrice, price.getSiteCouponPrice());
            siteCouponPoint = CurrencyUtil.add(siteCouponPoint, price.getSiteCouponPoint());
            siteCouponCommission = CurrencyUtil.add(siteCouponCommission, price.getSiteCouponCommission());

            flowPrice = CurrencyUtil.add(flowPrice, price.getFlowPrice());
            billPrice = CurrencyUtil.add(billPrice, price.getBillPrice());

        }
        originPriceDetail.setGoodsPrice(goodsPrice);
        originPriceDetail.setFreightPrice(freightPrice);
        originPriceDetail.setPayPoint(payPoint);
        originPriceDetail.setUpdatePrice(updatePrice);
        originPriceDetail.setDiscountPrice(discountPrice);

        originPriceDetail.setDistributionCommission(distributionCommission);
        originPriceDetail.setPlatFormCommission(platFormCommission);

        originPriceDetail.setSiteCouponPrice(siteCouponPrice);
        originPriceDetail.setSiteCouponPoint(siteCouponPoint);
        originPriceDetail.setSiteCouponCommission(siteCouponCommission);

        originPriceDetail.setFlowPrice(flowPrice);
        originPriceDetail.setBillPrice(billPrice);

        return originPriceDetail;
    }

    /**
     * 累加
     *
     * @param priceDetailDTOS
     * @return
     */
    public static Double sumGoodsPrice(List<PriceDetailDTO> priceDetailDTOS) {


        double goodsPrice = 0d;

        for (PriceDetailDTO price : priceDetailDTOS) {

            goodsPrice = CurrencyUtil.add(goodsPrice, price.getGoodsPrice());

        }

        return goodsPrice;
    }

    /**
     * 自身计价
     */
    public void count() {
        this.flowPrice = CurrencyUtil.sub(CurrencyUtil.add(goodsPrice, freightPrice), discountPrice);
        //this.billPrice = CurrencyUtil.sub(CurrencyUtil.sub(CurrencyUtil.sub(flowPrice, platFormCommission)), distributionCommission);
    }
}
