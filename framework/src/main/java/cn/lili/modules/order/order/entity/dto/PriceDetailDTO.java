package cn.lili.modules.order.order.entity.dto;


import cn.lili.common.utils.CurrencyUtil;
import cn.lili.modules.promotion.entity.vos.PromotionSkuVO;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * 商品项、店铺订单及交易层级共用的价格明细，承载金额计算结果、优惠来源及下单规则快照。
 * 金额单位均为元，积分单位为分值，比例字段使用百分数或折数时分别说明。
 * 多数金额setter会立即重算派生金额；汇总方法累加已计算结果，不累加比例或覆盖来源快照。
 *
 * @author Chopper
 * @since 2020/11/17 7:25 下午
 */
@Data
public class PriceDetailDTO implements Serializable {


    private static final long serialVersionUID = 8808470688518188146L;
    /**
     * 订单或商品项原始总金额，单位元，用于改价及优惠展示；读取为0时回退到当前流水金额。
     */
    @Schema(description = "订单原始总价格")
    private Double originalPrice;

    /**
     * 当前层级商品总金额基数，单位元；会员优惠计入discountPrice，不能同时从此基数重复扣减。
     */
    @Schema(description = "商品总金额（商品原价）")
    private Double goodsPrice;

    /**
     * 当前层级配送费用，单位元，计入买家实付金额。
     */
    @Schema(description = "配送费")
    private Double freightPrice;

    //============discount price============

    /**
     * 购买积分兑换商品所支付的积分数量，不是购物赠送积分，不直接作为现金金额计算。
     */
    @Schema(description = "支付积分")
    private Long payPoint;

    /**
     * 活动优惠合计，单位元，包含付费会员商品优惠；优惠券金额另存couponPrice，不能在此重复累计。
     */
    @Schema(description = "优惠金额")
    private Double discountPrice;

    /**
     * 逐项优惠来源明细，记录促销类型、来源主键及涉及商品；随商品项逐层汇总。
     */
    @Schema(description = "优惠详情")
    private List<DiscountPriceItem> discountPriceDetail;

    /**
     * 优惠券实际抵扣合计，单位元，包含适用的平台券及商家券抵扣。
     */
    @Schema(description = "优惠券金额")
    private Double couponPrice;

    /**
     * 礼品卡抵扣金额，单位元；减少现金实付，商家结算及平台佣金计算时补回。
     */
    @Schema(description = "礼品卡抵扣金额")
    private Double giftCardPrice;

    /**
     * 平台承担的付费会员商品优惠金额，单位元，已计入discountPrice。
     * 商家结算基数补回此金额，商品项汇总时逐项累加，历史JSON缺失或空值按0处理。
     */
    @Schema(description = "平台承担的付费会员商品优惠，加入优惠总额但结算时补回商家")
    private Double paidMemberSubsidy = 0D;
    /**
     * 读取平台会员补贴，兼容历史价格JSON缺失或显式null。
     *
     * @return 会员补贴金额，单位元，空值为0
     */
    public Double getPaidMemberSubsidy() { return paidMemberSubsidy == null ? 0D : paidMemberSubsidy; }

    //===========end discount price =============


    //=========distribution==========

    /**
     * 当前层级的分销佣金支出，单位元，从商家结算金额中扣除。
     */
    @Schema(description = "单品分销返现支出")
    private Double distributionCommission;


    /**
     * 平台交易佣金比例，单位百分数，例如10表示10%；按现金实付加礼品卡及会员补贴的基数计佣。
     */
    @Schema(description = "平台收取交易佣金比例")
    private Double platFormCommissionPoint;


    /**
     * 平台交易佣金金额，单位元；佣金比例大于0时由结算基数乘比例再除100计算。
     */
    @Schema(description = "平台交易佣金（元）=(flowPrice+giftCardPrice+paidMemberSubsidy)*platFormCommissionPoint/100，比例大于0时重算")
    private Double platFormCommission;

    //=========end distribution==========


    //========= platform coupon==========

    /**
     * 实际使用的平台优惠券金额，单位元，是couponPrice中的平台券部分，不能再单独扣减实付。
     */
    @Schema(description = "平台优惠券 使用金额")
    private Double siteCouponPrice;

    /**
     * 平台优惠券的商家承担比例，单位百分数，来自优惠券storeCommission；例如20表示商家承担20%。
     * 与StoreFlow中经100减本比例得到的平台承担比例区分使用。
     */
    @Schema(description = "平台优惠券商家承担比例，百分数，例如20表示20%")
    private Double siteCouponPoint;

    /**
     * 平台券按商家承担比例派生的承担金额，单位元；此DTO内与StoreFlow的平台承担金额口径不同。
     * 当前两个关联setter的公式不一致，具体行为见对应方法，需统一后才可作为稳定的计算依据。
     */
    @Schema(description = "平台优惠券商家承担金额（元），当前两个关联setter公式存在差异，见方法注释")
    private Double siteCouponCommission;
    //=========end platform coupon==========

    //========= update price ==========

    /**
     * 订单人工改价差额，单位元，正数加价、负数减价，不是修改后的订单总金额。
     */
    @Schema(description = "人工改价差额（元），正数加价、负数减价")
    private Double updatePrice;

    //=========end update price==========

    /**
     * 现金流水金额，单位元，由商品金额加运费减活动优惠、优惠券、礼品卡，再加人工改价差额计算。
     * 会员补贴已包含在活动优惠中，不再次扣减；公开读取时空值或非正数按0返回。
     */
    @Schema(description = "流水金额(入账 出帐金额) = " +
            "goodsPrice(商品总金额（商品原价）) + freightPrice(配送费) - " +
            "discountPrice(活动优惠金额) - couponPrice(优惠券优惠金额) + updatePrice(订单修改金额)-giftCardPrice(礼品卡抵扣金额)")
    private Double flowPrice;

    /**
     * 特殊活动指定的商家结算金额，单位元；大于0时优先作为最终结算额，不按普通佣金公式计算。
     */
    @Schema(description = "结算价格 与 商家/供应商 结算价格（例如积分商品/砍价商品）")
    private Double settlementPrice;

    /**
     * 最终商家结算金额，单位元；普通订单为现金实付加礼品卡、会员补贴后减平台及分销佣金。
     * 特殊指定结算金额大于0时取settlementPrice。
     */
    @Schema(description = "最终结算金额(普通订单)=结算计费基数-platFormCommission-distributionCommission；积分/砍价且settlementPrice>0时直接取settlementPrice")
    private Double billPrice;

    /**
     * 当前商品项参与的促销活动及活动主键列表，供促销校验和库存检查使用；金额汇总不合并此列表。
     */
    @Schema(description = "参与的促销活动")
    private List<PromotionSkuVO> joinPromotion;


    /**
     * 以单条优惠明细替换整个明细列表，而非追加。
     *
     * @param discountPriceItem 需要保留的单条优惠来源
     */
    public void setDiscountPriceItem(DiscountPriceItem discountPriceItem) {
        List<DiscountPriceItem> discountPriceItems = new ArrayList<>();
        discountPriceItems.add(discountPriceItem);
        this.discountPriceDetail = discountPriceItems;
    }

    /**
     * 追加一条优惠来源明细，不直接修改优惠合计或重算金额。
     *
     * @param discountPriceItem 待追加的优惠明细
     */
    public void addDiscountPriceItem(DiscountPriceItem discountPriceItem) {
        discountPriceDetail.add(discountPriceItem);
    }


    /**
     * 读取原始总金额，原值为0时回退当前流水金额，负数按0返回。
     *
     * @return 当前层级原始总金额，单位元
     */
    public Double getOriginalPrice() {
        if (originalPrice == 0D) {
            return flowPrice;
        }
        if (originalPrice < 0) {
            return 0d;
        }
        return originalPrice;
    }

    /**
     * 初始化金额、积分及比例为0，创建优惠与促销明细列表。
     * 平台会员补贴初始化为0，供后续会员价计算时写入。
     */
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
     * 设置人工改价差额并重新计算现金实付、佣金和结算金额。
     *
     * @param updatePrice 改价差额，单位元，正数加价、负数减价
     */
    public void setUpdatePrice(Double updatePrice) {
        this.updatePrice = updatePrice;
        this.recount();
    }


    /**
     * 计算商家结算及平台交易佣金计费基数，补回平台承担的礼品卡及会员商品优惠。
     *
     * @return 现金流水加礼品卡抵扣加会员补贴，单位元
     */
    private double commissionSettlementBase() {
        double giftCard = giftCardPrice != null ? giftCardPrice : 0;
        return CurrencyUtil.add(flowPrice, giftCard, getPaidMemberSubsidy());
    }

    /**
     * 按当前字段重新计算现金流水、平台交易佣金及最终商家结算金额。
     * 实付为商品金额加运费减活动优惠、券及礼品卡，再加改价差额；佣金基数补回礼品卡与会员补贴。
     * 佣金比例大于0时重算佣金，特殊结算金额大于0时直接使用该金额；此方法不重新计算优惠分摊。
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
     * 累加一份已经计算完成的下级价格明细，不重新按汇总比例计算佣金。
     * 金额、支付积分及会员补贴求和，优惠明细追加；平台券比例取本次入参值。
     * 只汇总金额，不将优惠明细中的促销标识直接合并为单一来源。
     *
     * @param priceDetailDTO 已计算完成的商品项或店铺价格明细
     */
    public void increase(PriceDetailDTO priceDetailDTO) {

        originalPrice = CurrencyUtil.add(originalPrice, priceDetailDTO.getOriginalPrice());
        goodsPrice = CurrencyUtil.add(goodsPrice, priceDetailDTO.getGoodsPrice());
        freightPrice = CurrencyUtil.add(freightPrice, priceDetailDTO.getFreightPrice());

        payPoint = payPoint + priceDetailDTO.getPayPoint();
        discountPrice = CurrencyUtil.add(discountPrice, priceDetailDTO.getDiscountPrice());
        couponPrice = CurrencyUtil.add(couponPrice, priceDetailDTO.getCouponPrice());
        giftCardPrice = CurrencyUtil.add(giftCardPrice, priceDetailDTO.getGiftCardPrice());
        paidMemberSubsidy = CurrencyUtil.add(getPaidMemberSubsidy(), priceDetailDTO.getPaidMemberSubsidy());

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
     * 依次累加多份价格明细，保留各商品项已经计算的舍入结果。
     * 不会清空当前值，重复传入同一批数据会重复累加。
     *
     * @param priceDetailDTOS 待汇总的下级价格明细列表
     */
    public void accumulationPriceDTO(List<PriceDetailDTO> priceDetailDTOS) {
        for (PriceDetailDTO price : priceDetailDTOS) {
            this.increase(price);
        }
    }


    /**
     * 读取商品总金额，空值或非正数按0返回。
     *
     * @return 商品总金额，单位元
     */
    public Double getGoodsPrice() {
        if (goodsPrice == null || goodsPrice <= 0) {
            return 0D;
        }
        return goodsPrice;
    }

    /**
     * 读取配送费用，空值或非正数按0返回。
     *
     * @return 配送费用，单位元
     */
    public Double getFreightPrice() {
        if (freightPrice == null || freightPrice <= 0) {
            return 0D;
        }
        return freightPrice;
    }

    /**
     * 读取积分兑换商品所支付的积分，空值或非正数按0返回。
     *
     * @return 支付积分数量
     */
    public Long getPayPoint() {
        if (payPoint == null || payPoint <= 0) {
            return 0L;
        }
        return payPoint;
    }

    /**
     * 读取活动优惠金额，空值或非正数按0返回。
     *
     * @return 活动优惠金额，单位元
     */
    public Double getDiscountPrice() {
        if (discountPrice == null || discountPrice <= 0) {
            return 0D;
        }
        return discountPrice;
    }

    /**
     * 读取优惠券抵扣金额，空值或非正数按0返回。
     *
     * @return 优惠券抵扣金额，单位元
     */
    public Double getCouponPrice() {
        if (couponPrice == null || couponPrice <= 0) {
            return 0D;
        }
        return couponPrice;
    }

    /**
     * 读取礼品卡抵扣金额，空值或非正数按0返回。
     *
     * @return 礼品卡抵扣金额，单位元
     */
    public Double getGiftCardPrice() {
        if (giftCardPrice == null || giftCardPrice <= 0) {
            return 0D;
        }
        return giftCardPrice;
    }

    /**
     * 读取分销佣金支出，空值或非正数按0返回。
     *
     * @return 分销佣金支出，单位元
     */
    public Double getDistributionCommission() {
        if (distributionCommission == null || distributionCommission <= 0) {
            return 0D;
        }
        return distributionCommission;
    }

    /**
     * 读取平台交易佣金，空值或非正数按0返回。
     *
     * @return 平台交易佣金，单位元
     */
    public Double getPlatFormCommission() {
        if (platFormCommission == null || platFormCommission <= 0) {
            return 0D;
        }
        return platFormCommission;
    }

    /**
     * 读取平台优惠券抵扣金额，空值或非正数按0返回。
     *
     * @return 平台优惠券抵扣金额，单位元
     */
    public Double getSiteCouponPrice() {
        if (siteCouponPrice == null || siteCouponPrice <= 0) {
            return 0D;
        }
        return siteCouponPrice;
    }

    /**
     * 读取平台券商家承担比例，空值或非正数按0返回。
     *
     * @return 商家承担百分数，例如20表示20%
     */
    public Double getSiteCouponPoint() {
        if (siteCouponPoint == null || siteCouponPoint <= 0) {
            return 0D;
        }
        return siteCouponPoint;
    }

    /**
     * 读取当前DTO口径的平台券承担金额，空值或非正数按0返回。
     *
     * @return 当前DTO口径的平台券承担金额，单位元
     */
    public Double getSiteCouponCommission() {
        if (siteCouponCommission == null || siteCouponCommission <= 0) {
            return 0D;
        }
        return siteCouponCommission;
    }


    /**
     * 读取现金流水金额，空值或非正数按0返回。
     *
     * @return 现金流水金额，单位元
     */
    public Double getFlowPrice() {
        if (flowPrice == null || flowPrice <= 0) {
            return 0D;
        }
        return flowPrice;
    }

    /**
     * 读取特殊指定结算金额，空值或非正数按0返回。
     *
     * @return 特殊指定结算金额，单位元
     */
    public Double getSettlementPrice() {
        if (settlementPrice == null || settlementPrice <= 0) {
            return 0D;
        }
        return settlementPrice;
    }

    /**
     * 读取最终商家结算金额，空值或非正数按0返回。
     *
     * @return 最终商家结算金额，单位元
     */
    public Double getBillPrice() {
        if (billPrice == null || billPrice <= 0) {
            return 0D;
        }
        return billPrice;
    }

    /**
     * 读取人工改价差额，空值按0返回，保留负数减价。
     *
     * @return 改价差额，单位元
     */
    public Double getUpdatePrice() {
        if (updatePrice == null) {
            return 0D;
        }
        return updatePrice;
    }


    /**
     * 设置平台券抵扣金额，并在承担比例非0时计算对应承担金额。
     * 当前实现为金额直接乘百分数，未除100，与setSiteCouponPoint的公式不一致。
     * 本方法不调用金额重算，后续需要统一两个入口的比例单位与计算规则。
     *
     * @param siteCouponPrice 平台优惠券实际抵扣金额，单位元
     */
    public void setSiteCouponPrice(Double siteCouponPrice) {
        this.siteCouponPrice = siteCouponPrice;

        if (siteCouponPoint != null && siteCouponPoint != 0) {
            this.siteCouponCommission = CurrencyUtil.mul(siteCouponPrice, siteCouponPoint);
        }
    }

    /**
     * 设置平台券商家承担比例，并按平台券金额乘比例除100计算承担金额。
     * 比例为0时当前实现保留旧承担金额；本方法不调用整体价格重算。
     *
     * @param siteCouponPoint 商家承担比例，单位百分数，例如20表示20%
     */
    public void setSiteCouponPoint(Double siteCouponPoint) {
        this.siteCouponPoint = siteCouponPoint;

        if (siteCouponPoint != null && siteCouponPoint != 0) {
            this.siteCouponCommission = CurrencyUtil.div(CurrencyUtil.mul(siteCouponPrice, siteCouponPoint), 100);
        }
    }


    /**
     * 设置商品金额基数并重算现金流水、佣金和结算金额。
     *
     * @param goodsPrice 商品金额，单位元，会员补贴不能在此重复扣减
     */
    public void setGoodsPrice(Double goodsPrice) {
        this.goodsPrice = goodsPrice;
        this.recount();
    }

    /**
     * 设置配送费用并重算派生金额。
     *
     * @param freightPrice 配送费用，单位元
     */
    public void setFreightPrice(Double freightPrice) {
        this.freightPrice = freightPrice;
        this.recount();
    }

    /**
     * 设置积分兑换商品的支付积分，不参与现金金额重算。
     *
     * @param payPoint 支付积分数量
     */
    public void setPayPoint(Long payPoint) {
        this.payPoint = payPoint;
    }

    /**
     * 设置活动优惠合计，限制券与活动优惠组合后重算派生金额。
     *
     * @param discountPrice 活动优惠总额，单位元，包含会员商品优惠
     */
    public void setDiscountPrice(Double discountPrice) {
        this.discountPrice = discountPrice;
        promotionPriceHandler();
        this.recount();
    }

    /**
     * 设置优惠券抵扣合计，执行优惠金额限制后重算派生金额。
     *
     * @param couponPrice 优惠券实际抵扣总额，单位元
     */
    public void setCouponPrice(Double couponPrice) {
        this.couponPrice = couponPrice;
        promotionPriceHandler();
        this.recount();
    }

    /**
     * 设置礼品卡抵扣并重算派生金额，结算基数中补回该平台承担金额。
     *
     * @param giftCardPrice 礼品卡抵扣金额，单位元
     */
    public void setGiftCardPrice(Double giftCardPrice) {
        this.giftCardPrice = giftCardPrice;
        this.recount();
    }

    /**
     * 在商品金额已就绪时检查活动优惠与券抵扣之和，超过商品金额则调整券抵扣金额。
     * 调整会再次调用券金额setter；当前逻辑未单独限制活动优惠本身超过商品金额的场景。
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

    /**
     * 设置分销佣金支出并重算最终商家结算金额。
     *
     * @param distributionCommission 分销佣金，单位元
     */
    public void setDistributionCommission(Double distributionCommission) {
        this.distributionCommission = distributionCommission;
        this.recount();
    }

    /**
     * 设置平台交易佣金百分比，并按当前结算基数重算佣金与商家结算。
     *
     * @param platFormCommissionPoint 佣金比例，单位百分数，例如10表示10%
     */
    public void setPlatFormCommissionPoint(Double platFormCommissionPoint) {
        this.platFormCommissionPoint = platFormCommissionPoint;
        this.recount();
    }

    /**
     * 写入平台佣金后重算；当佣金比例大于0时会按比例覆盖本次传入值。
     *
     * @param platFormCommission 平台交易佣金金额，单位元
     */
    public void setPlatFormCommission(Double platFormCommission) {
        this.platFormCommission = platFormCommission;
        this.recount();
    }

    /**
     * 设置平台券承担金额并触发整体金额重算，该字段不直接参与当前recount公式。
     *
     * @param siteCouponCommission 当前DTO口径的平台券承担金额，单位元
     */
    public void setSiteCouponCommission(Double siteCouponCommission) {
        this.siteCouponCommission = siteCouponCommission;
        this.recount();
    }

    /**
     * 写入流水金额后立即按当前组成字段重算，本次传入值会被recount覆盖。
     *
     * @param flowPrice 传入的现金流水金额，单位元
     */
    public void setFlowPrice(Double flowPrice) {
        this.flowPrice = flowPrice;
        this.recount();
    }

    /**
     * 设置特殊指定结算金额并重算；大于0时优先使用该金额作为商家结算。
     *
     * @param settlementPrice 特殊活动约定结算金额，单位元
     */
    public void setSettlementPrice(Double settlementPrice) {
        this.settlementPrice = settlementPrice;
        this.recount();
    }

    /**
     * 写入商家结算额后立即重算，本次传入值会被当前结算规则覆盖。
     *
     * @param billPrice 传入的商家结算金额，单位元
     */
    public void setBillPrice(Double billPrice) {
        this.billPrice = billPrice;
        this.recount();
    }
}
