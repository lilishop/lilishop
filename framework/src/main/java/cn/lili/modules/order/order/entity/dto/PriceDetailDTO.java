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
 * @since 2020/11/17 7:25 下午
 */
@Data
public class PriceDetailDTO implements Serializable {


    private static final long serialVersionUID = 8808470688518188146L;
    /**
     * 订单原始总价格
     * 用于订单价格修改金额计算使用
     */
    @ApiModelProperty(value = "订单原始总价格")
    private Double originalPrice;

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

    @ApiModelProperty(value = "结算价格 与 商家/供应商 结算价格（例如积分商品/砍价商品）")
    private Double settlementPrice;

    @ApiModelProperty(value = "最终结算金额 = flowPrice - platFormCommission - distributionCommission")
    private Double billPrice;

    /**
     * 参与的促销活动
     */
    @ApiModelProperty(value = "参与的促销活动")
    private List<BasePromotion> joinPromotion;


    public Double getOriginalPrice() {
        if (originalPrice == 0D) {
            return flowPrice;
        }
        return originalPrice;
    }

    public PriceDetailDTO() {
        originalPrice = 0d;
        goodsPrice = 0d;
        freightPrice = 0d;

        payPoint = 0;
        discountPrice = 0d;
        couponPrice = 0d;

        distributionCommission = 0d;
        platFormCommission = 0d;

        siteCouponPrice = 0d;
        siteCouponPoint = 0d;
        siteCouponCommission = 0d;

        updatePrice = 0d;

        flowPrice = 0d;
        billPrice = 0d;
        settlementPrice = 0d;


        joinPromotion = new ArrayList<>();
    }


    /**
     * 写入修改金额，自动计算订单各个金额
     *
     * @param updatePrice 修改后的订单金额
     */
    public void setUpdatePrice(Double updatePrice) {
        this.updatePrice = updatePrice;
    }


    /**
     * 写入佣金比例，计算结算金额
     *
     * @param commission 佣金比例
     */
    public void setCommission(Double commission) {

        //流水金额(入账 出帐金额) = goodsPrice + freight - （discountPrice + couponPrice）
        this.flowPrice = CurrencyUtil.sub(
                CurrencyUtil.add(goodsPrice, freightPrice),
                CurrencyUtil.add(discountPrice,
                        couponPrice != null ? couponPrice : 0));

        //计算平台佣金  流水金额*平台佣金比例
        if (commission != null && commission > 0) {
            platFormCommission = CurrencyUtil.div(CurrencyUtil.mul(flowPrice, commission), 100);
        }
        countBill();
    }

    public void countBill() {
        //如果是普通订单最终结算金额 = flowPrice - platFormCommission - distributionCommission
        billPrice = CurrencyUtil.sub(CurrencyUtil.sub(flowPrice, platFormCommission), distributionCommission);

    }

    /**
     * 累加
     *
     * @param priceDetailDTOS
     * @return
     */
    public void accumulationPriceDTO(List<PriceDetailDTO> priceDetailDTOS) {


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
        this.setGoodsPrice(goodsPrice);
        this.setFreightPrice(freightPrice);
        this.setPayPoint(payPoint);
        this.setUpdatePrice(updatePrice);
        this.setDiscountPrice(discountPrice);

        this.setDistributionCommission(distributionCommission);
        this.setPlatFormCommission(platFormCommission);

        this.setSiteCouponPrice(siteCouponPrice);
        this.setSiteCouponPoint(siteCouponPoint);
        this.setSiteCouponCommission(siteCouponCommission);

        this.setFlowPrice(flowPrice);
        this.setBillPrice(billPrice);
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

    public Double getGoodsPrice() {
        if (goodsPrice == null || goodsPrice <= 0) {
            return 0D;
        }
        return goodsPrice;
    }

    public Double getFreightPrice() {
        if (freightPrice == null || freightPrice <= 0) {
            return 0D;
        }
        return freightPrice;
    }

    public Integer getPayPoint() {
        if (payPoint == null || payPoint <= 0) {
            return 0;
        }
        return payPoint;
    }

    public Double getDiscountPrice() {
        if (discountPrice == null || discountPrice <= 0) {
            return 0D;
        }
        return discountPrice;
    }

    public Double getCouponPrice() {
        if (couponPrice == null || couponPrice <= 0) {
            return 0D;
        }
        return couponPrice;
    }

    public Double getDistributionCommission() {
        if (distributionCommission == null || distributionCommission <= 0) {
            return 0D;
        }
        return distributionCommission;
    }

    public Double getPlatFormCommission() {
        if (platFormCommission == null || platFormCommission <= 0) {
            return 0D;
        }
        return platFormCommission;
    }

    public Double getSiteCouponPrice() {
        if (siteCouponPrice == null || siteCouponPrice <= 0) {
            return 0D;
        }
        return siteCouponPrice;
    }

    public Double getSiteCouponPoint() {
        if (siteCouponPoint == null || siteCouponPoint <= 0) {
            return 0D;
        }
        return siteCouponPoint;
    }

    public Double getSiteCouponCommission() {
        if (siteCouponCommission == null || siteCouponCommission <= 0) {
            return 0D;
        }
        return siteCouponCommission;
    }


    public Double getFlowPrice() {
        if (flowPrice == null || flowPrice <= 0) {
            return 0D;
        }
        return flowPrice;
    }

    public Double getSettlementPrice() {
        if (settlementPrice == null || settlementPrice <= 0) {
            return 0D;
        }
        return settlementPrice;
    }

    public Double getBillPrice() {
        if (billPrice == null || billPrice <= 0) {
            return 0D;
        }
        return billPrice;
    }
}
