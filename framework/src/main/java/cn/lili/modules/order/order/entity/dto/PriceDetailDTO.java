package cn.lili.modules.order.order.entity.dto;


import cn.lili.common.utils.CurrencyUtil;
import cn.lili.modules.promotion.entity.vos.PromotionSkuVO;
import io.swagger.v3.oas.annotations.media.Schema;
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
    @Schema(description = "订单原始总价格")
    private Double originalPrice;

    @Schema(description = "商品总金额（商品原价）")
    private Double goodsPrice;

    @Schema(description = "配送费")
    private Double freightPrice;

    //============discount price============

    @Schema(description = "支付积分")
    private Long payPoint;

    @Schema(description = "优惠金额")
    private Double discountPrice;

    @Schema(description = "优惠详情")
    private List<DiscountPriceItem> discountPriceDetail;

    @Schema(description = "优惠券金额")
    private Double couponPrice;

    @Schema(description = "礼品卡抵扣金额")
    private Double giftCardPrice;

    //===========end discount price =============


    //=========distribution==========

    @Schema(description = "单品分销返现支出")
    private Double distributionCommission;


    @Schema(description = "平台收取交易佣金比例")
    private Double platFormCommissionPoint;


    @Schema(description = "平台收取交易佣金=(结算计费基数 * platFormCommissionPoint)/100；有礼品卡时基数=flowPrice+giftCardPrice，否则=flowPrice")
    private Double platFormCommission;

    //=========end distribution==========


    //========= platform coupon==========

    @Schema(description = "平台优惠券 使用金额")
    private Double siteCouponPrice;

    @Schema(description = "站点优惠券佣金比例")
    private Double siteCouponPoint;

    @Schema(description = "站点优惠券佣金")
    private Double siteCouponCommission;
    //=========end platform coupon==========

    //========= update price ==========

    @Schema(description = "订单修改金额")
    private Double updatePrice;

    //=========end update price==========

    @Schema(description = "流水金额(入账 出帐金额) = " +
            "goodsPrice(商品总金额（商品原价）) + freightPrice(配送费) - " +
            "discountPrice(活动优惠金额) - couponPrice(优惠券优惠金额) + updatePrice(订单修改金额)-giftCardPrice(礼品卡抵扣金额)")
    private Double flowPrice;

    @Schema(description = "结算价格 与 商家/供应商 结算价格（例如积分商品/砍价商品）")
    private Double settlementPrice;

    @Schema(description = "最终结算金额(普通订单)=结算计费基数-platFormCommission-distributionCommission；积分/砍价且settlementPrice>0时直接取settlementPrice")
    private Double billPrice;

    /**
     * 参与的促销活动
     */
    @Schema(description = "参与的促销活动")
    private List<PromotionSkuVO> joinPromotion;


    /**
     * 设置促销详情
     *
     * @param discountPriceItem 促销信息
     */
    public void setDiscountPriceItem(DiscountPriceItem discountPriceItem) {
        List<DiscountPriceItem> discountPriceItems = new ArrayList<>();
        discountPriceItems.add(discountPriceItem);
        this.discountPriceDetail = discountPriceItems;
    }

    /**
     * 设置促销详情
     *
     * @param discountPriceItem 促销信息
     */
    public void addDiscountPriceItem(DiscountPriceItem discountPriceItem) {
        discountPriceDetail.add(discountPriceItem);
    }


    public Double getOriginalPrice() {
        if (originalPrice == 0D) {
            return flowPrice;
        }
        if (originalPrice < 0) {
            return 0d;
        }
        return originalPrice;
    }

    public PriceDetailDTO() {
        originalPrice = 0d;
        goodsPrice = 0d;
        freightPrice = 0d;

        payPoint = 0L;
        discountPrice = 0d;
        couponPrice = 0d;
        giftCardPrice = 0d;

        distributionCommission = 0d;
        platFormCommissionPoint = 0d;
        platFormCommission = 0d;

        siteCouponPrice = 0d;
        siteCouponPoint = 0d;
        siteCouponCommission = 0d;

        updatePrice = 0d;

        flowPrice = 0d;
        billPrice = 0d;
        settlementPrice = 0d;

        discountPriceDetail = new ArrayList<>();

        joinPromotion = new ArrayList<>();
    }


    /**
     * 写入修改金额，自动计算订单各个金额
     *
     * @param updatePrice 修改后的订单金额
     */
    public void setUpdatePrice(Double updatePrice) {
        this.updatePrice = updatePrice;
        this.recount();
    }


    /**
     * 商家结算/平台佣金计费基数：应付现金(flowPrice) + 礼品卡抵扣（礼品卡由平台承担，仍按抵扣前金额计佣）
     */
    private double commissionSettlementBase() {
        double giftCard = giftCardPrice != null ? giftCardPrice : 0;
        if (giftCard > 0) {
            return CurrencyUtil.add(flowPrice, giftCard);
        }
        return flowPrice;
    }

    /**
     * 全部重新计算
     */
    public void recount() {
        //流水金额(入账 出帐金额) = goodsPrice + freight - discountPrice - couponPrice - giftCardPrice + updatePrice
        this.flowPrice = CurrencyUtil.sub(
                CurrencyUtil.add(goodsPrice, freightPrice),
                CurrencyUtil.add(discountPrice,
                        couponPrice != null ? couponPrice : 0,
                        giftCardPrice != null ? giftCardPrice : 0));
        if (updatePrice != 0) {
            flowPrice = CurrencyUtil.add(flowPrice, updatePrice);
        }

        double settlementBase = commissionSettlementBase();

        // 平台佣金按结算计费基数计算（礼品卡全额抵扣时 flowPrice 为 0，仍可按订单原金额计佣）
        if (platFormCommissionPoint != null && getPlatFormCommissionPoint() > 0) {
            platFormCommission = CurrencyUtil.div(CurrencyUtil.mul(settlementBase, platFormCommissionPoint), 100);
        }

        //如果结算信息包含结算金额，则最终结算金额直接等于该交易 平台与商户的结算金额
        if (settlementPrice > 0) {
            billPrice = settlementPrice;
        } else {
            // 普通订单：商家结算 = 计费基数 - 平台佣金 - 分销佣金
            billPrice = CurrencyUtil.sub(settlementBase, platFormCommission, distributionCommission);
        }
    }

    /**
     * 累加金额
     */
    public void increase(PriceDetailDTO priceDetailDTO) {

        originalPrice = CurrencyUtil.add(originalPrice, priceDetailDTO.getOriginalPrice());
        goodsPrice = CurrencyUtil.add(goodsPrice, priceDetailDTO.getGoodsPrice());
        freightPrice = CurrencyUtil.add(freightPrice, priceDetailDTO.getFreightPrice());

        payPoint = payPoint + priceDetailDTO.getPayPoint();
        discountPrice = CurrencyUtil.add(discountPrice, priceDetailDTO.getDiscountPrice());
        couponPrice = CurrencyUtil.add(couponPrice, priceDetailDTO.getCouponPrice());
        giftCardPrice = CurrencyUtil.add(giftCardPrice, priceDetailDTO.getGiftCardPrice());

        distributionCommission = CurrencyUtil.add(distributionCommission, priceDetailDTO.getDistributionCommission());
        platFormCommission = CurrencyUtil.add(platFormCommission, priceDetailDTO.getPlatFormCommission());

        siteCouponPrice = CurrencyUtil.add(siteCouponPrice, priceDetailDTO.getSiteCouponPrice());
        //平台优惠券一笔交易只有一个，所以这里直接移值就好了
        siteCouponPoint = priceDetailDTO.getSiteCouponPoint();
        siteCouponCommission = CurrencyUtil.add(siteCouponCommission, priceDetailDTO.getSiteCouponCommission());

        updatePrice = CurrencyUtil.add(updatePrice, priceDetailDTO.getUpdatePrice());

        flowPrice = CurrencyUtil.add(flowPrice, priceDetailDTO.getFlowPrice());
        billPrice = CurrencyUtil.add(billPrice, priceDetailDTO.getBillPrice());
        settlementPrice = CurrencyUtil.add(settlementPrice, priceDetailDTO.getSettlementPrice());

        discountPriceDetail.addAll(priceDetailDTO.getDiscountPriceDetail());

    }

    /**
     * 批量累加
     *
     * @param priceDetailDTOS
     * @return
     */
    public void accumulationPriceDTO(List<PriceDetailDTO> priceDetailDTOS) {
        for (PriceDetailDTO price : priceDetailDTOS) {
            this.increase(price);
        }
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

    public Long getPayPoint() {
        if (payPoint == null || payPoint <= 0) {
            return 0L;
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

    public Double getGiftCardPrice() {
        if (giftCardPrice == null || giftCardPrice <= 0) {
            return 0D;
        }
        return giftCardPrice;
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

    public Double getUpdatePrice() {
        if (updatePrice == null) {
            return 0D;
        }
        return updatePrice;
    }


    public void setSiteCouponPrice(Double siteCouponPrice) {
        this.siteCouponPrice = siteCouponPrice;

        if (siteCouponPoint != null && siteCouponPoint != 0) {
            this.siteCouponCommission = CurrencyUtil.mul(siteCouponPrice, siteCouponPoint);
        }
    }

    public void setSiteCouponPoint(Double siteCouponPoint) {
        this.siteCouponPoint = siteCouponPoint;

        if (siteCouponPoint != null && siteCouponPoint != 0) {
            this.siteCouponCommission = CurrencyUtil.div(CurrencyUtil.mul(siteCouponPrice, siteCouponPoint), 100);
        }
    }


    public void setGoodsPrice(Double goodsPrice) {
        this.goodsPrice = goodsPrice;
        this.recount();
    }

    public void setFreightPrice(Double freightPrice) {
        this.freightPrice = freightPrice;
        this.recount();
    }

    public void setPayPoint(Long payPoint) {
        this.payPoint = payPoint;
    }

    public void setDiscountPrice(Double discountPrice) {
        this.discountPrice = discountPrice;
        promotionPriceHandler();
        this.recount();
    }

    public void setCouponPrice(Double couponPrice) {
        this.couponPrice = couponPrice;
        promotionPriceHandler();
        this.recount();
    }

    public void setGiftCardPrice(Double giftCardPrice) {
        this.giftCardPrice = giftCardPrice;
        this.recount();
    }

    /**
     * 如果促销金额+优惠券金额大于商品金额问题处理
     */
    private void promotionPriceHandler() {
        if (couponPrice == null || discountPrice == null) {
            return;
        }
        // goodsPrice 未就绪时跳过（如 JSON 反序列化时 couponPrice 先于 goodsPrice 赋值）
        if (goodsPrice == null || goodsPrice <= 0) {
            return;
        }
        //如果订单优惠总额>商品金额，则处理一下数据，使得两数相加<=商品金额
        if (CurrencyUtil.add(couponPrice, discountPrice) > goodsPrice) {
            couponPrice = CurrencyUtil.sub(goodsPrice, discountPrice);
            this.setCouponPrice(couponPrice);
        }
    }

    public void setDistributionCommission(Double distributionCommission) {
        this.distributionCommission = distributionCommission;
        this.recount();
    }

    public void setPlatFormCommissionPoint(Double platFormCommissionPoint) {
        this.platFormCommissionPoint = platFormCommissionPoint;
        this.recount();
    }

    public void setPlatFormCommission(Double platFormCommission) {
        this.platFormCommission = platFormCommission;
        this.recount();
    }

    public void setSiteCouponCommission(Double siteCouponCommission) {
        this.siteCouponCommission = siteCouponCommission;
        this.recount();
    }

    public void setFlowPrice(Double flowPrice) {
        this.flowPrice = flowPrice;
        this.recount();
    }

    public void setSettlementPrice(Double settlementPrice) {
        this.settlementPrice = settlementPrice;
        this.recount();
    }

    public void setBillPrice(Double billPrice) {
        this.billPrice = billPrice;
        this.recount();
    }
}
