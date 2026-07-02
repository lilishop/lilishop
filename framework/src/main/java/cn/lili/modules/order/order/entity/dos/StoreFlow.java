package cn.lili.modules.order.order.entity.dos;

import cn.hutool.core.date.DateUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.utils.BeanUtil;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.common.utils.SnowFlake;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.modules.order.order.entity.dto.DiscountPriceItem;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.order.order.entity.enums.FlowTypeEnum;
import cn.lili.modules.order.order.entity.enums.OrderPromotionTypeEnum;
import cn.lili.modules.order.order.entity.enums.ProfitSharingStatusEnum;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.mybatis.BaseIdEntity;
import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 商家订单流水
 *
 * @author Chopper
 * @since 2020/11/17 7:31 下午
 */
@Data
@TableName("li_store_flow")
@Schema(description = "商家订单流水")
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class StoreFlow extends BaseIdEntity {

    private static final long serialVersionUID = -5998757398902747939L;

    @Schema(description = "流水编号")
    private String sn;

    @Schema(description = "订单sn")
    private String orderSn;

    @Schema(description = "子订单sn")
    private String orderItemSn;

    @Schema(description = "售后SN")
    private String refundSn;

    @Schema(description = "店铺id")
    private String storeId;

    @Schema(description = "店铺名称 ")
    private String storeName;

    @Schema(description = "客户id")
    private String memberId;

    @Schema(description = "客户名称")
    private String memberName;


    @Schema(description = "商品ID")
    private String goodsId;

    @Schema(description = "商品名称")
    private String goodsName;

    @Schema(description = "货品ID")
    private String skuId;

    @Schema(description = "图片")
    private String image;

    @Schema(description = "分类ID")
    private String categoryId;

    @Schema(description = "规格json")
    private String specs;


    /**
     * @see FlowTypeEnum
     */
    @Schema(description = "流水类型：PAY/REFUND 支付/退款", allowableValues = "PAY,REFUND")
    private String flowType;

    /**
     * @see cn.lili.modules.order.order.entity.enums.OrderPromotionTypeEnum
     */
    @Schema(description = "订单促销类型")
    private String orderPromotionType;

    @Schema(description = "积分活动商品结算价格")
    private Double pointSettlementPrice;

    @Schema(description = "砍价活动商品结算价格")
    private Double kanjiaSettlementPrice;

    @Schema(description = "限时直降补贴")
    private Double flashDiscountSubsidy;

    @Schema(description = "第N件优惠补贴")
    private Double nthItemSubsidy;

    @Schema(description = "平台优惠券 使用金额")
    private Double siteCouponPrice;

    @Schema(description = "站点优惠券补贴比例")
    private Double siteCouponPoint;

    @Schema(description = "站点优惠券补贴金额")
    private Double siteCouponCommission;

    @Schema(description = "礼品卡平台补贴金额（抵扣金额，由平台承担）")
    private Double giftCardSubsidyPrice;

    @Schema(description = "单品分销返现支出")
    private Double distributionRebate;

    @Schema(description = "平台收取交易佣金")
    private Double commissionPrice;

    @Schema(description = "流水金额（用户实际支付现金，已扣礼品卡）")
    private Double finalPrice;

    @Schema(description = "商家结算计费基数（有礼品卡时=finalPrice+giftCardSubsidyPrice）")
    private Double settlementBasePrice;

    @Schema(description = "最终结算金额（商家入账）")
    private Double billPrice;

    @Schema(description = "第三方交易流水号")
    private String transactionId;

    /**
     * @see PaymentMethodEnum
     */
    @Schema(description = "支付方式名称")
    private String paymentName;

    @Schema(description = "销售量")
    private Integer num;

    @CreatedDate
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @TableField(fill = FieldFill.INSERT)
    @Schema(description = "创建时间", hidden = true)
    private Date createTime;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @TableField(fill = FieldFill.INSERT)
    @Schema(description = "结算时间", hidden = true)
    private Date billTime;

    @Schema(description = "是否全部退款，true为全部退款")
    private Boolean fullRefund;

    /**
     * @see ProfitSharingStatusEnum
     */
    @Schema(description = "分账状态")
    private String profitSharingStatus;

    @Schema(description = "实际分账金额DTO", hidden = true)
    private String profitSharing;


    public StoreFlow(Order order, OrderItem item, FlowTypeEnum flowTypeEnum) {
        //获取订单促销类型,如果为促销订单则获取促销商品并获取结算价
        String promotionType = order.getOrderPromotionType();
        BeanUtil.copyProperties(item, this);

        //去掉orderitem的时间。
        this.setCreateTime(null);
        //入账
        this.setId(SnowFlake.getIdStr());
        this.setFlowType(flowTypeEnum.name());
        this.setSn(SnowFlake.createStr("SF"));
        this.setOrderSn(item.getOrderSn());
        this.setOrderItemSn(item.getSn());
        this.setStoreId(order.getStoreId());
        this.setStoreName(order.getStoreName());
        this.setMemberId(order.getMemberId());
        this.setMemberName(order.getMemberName());
        this.setGoodsName(item.getGoodsName());
        this.setOrderPromotionType(item.getPromotionType());
        //格式化订单价格详情
        PriceDetailDTO priceDetailDTO = JSONUtil.toBean(item.getPriceDetail(), PriceDetailDTO.class);
        //站点优惠券比例=最大比例(100)-店铺承担比例
        this.setSiteCouponPoint(CurrencyUtil.sub(100, priceDetailDTO.getSiteCouponPoint()));
        //平台优惠券 使用金额
        this.setSiteCouponPrice(priceDetailDTO.getSiteCouponPrice());
        //站点优惠券佣金（站点优惠券承担金额=优惠券金额 * (站点承担比例/100)）
        this.setSiteCouponCommission(CurrencyUtil.mul(this.getSiteCouponPrice(), CurrencyUtil.div(this.getSiteCouponPoint(), 100)));

        /**
         * @TODO 计算平台佣金
         */
        double giftCardSubsidy = priceDetailDTO.getGiftCardPrice();
        this.setGiftCardSubsidyPrice(giftCardSubsidy);
        this.setFinalPrice(item.getPriceDetailDTO().getFlowPrice());
        this.setSettlementBasePrice(giftCardSubsidy > 0
                ? CurrencyUtil.add(this.getFinalPrice(), giftCardSubsidy)
                : this.getFinalPrice());
        this.setCommissionPrice(item.getPriceDetailDTO().getPlatFormCommission());
        this.setDistributionRebate(item.getPriceDetailDTO().getDistributionCommission());
        this.setBillPrice(item.getPriceDetailDTO().getBillPrice());
        //兼容为空，以及普通订单操作
        if (CharSequenceUtil.isNotEmpty(promotionType)) {
            //如果为砍价活动，填写砍价结算价
            if (promotionType.equals(OrderPromotionTypeEnum.KANJIA.name())) {
                this.setKanjiaSettlementPrice(item.getPriceDetailDTO().getSettlementPrice());
            }
            //如果为积分活动，填写积分结算价
            else if (promotionType.equals(OrderPromotionTypeEnum.POINTS.name())) {
                this.setPointSettlementPrice(item.getPriceDetailDTO().getSettlementPrice());
            }
        }
        applyMarketingSubsidy(item.getPriceDetailDTO());
        //添加支付方式
        this.setPaymentName(order.getPaymentMethod());
        //添加第三方支付流水号
        this.setTransactionId(order.getReceivableNo());
        //默认结算时间180天
        if (flowTypeEnum.equals(FlowTypeEnum.PAY)) {
            this.billTime = DateUtil.offsetDay(new Date(), 180);
            this.fullRefund = false;
        }
    }

    private void applyMarketingSubsidy(PriceDetailDTO priceDetailDTO) {
        if (priceDetailDTO == null || priceDetailDTO.getDiscountPriceDetail() == null) {
            return;
        }
        double flash = 0D;
        double nth = 0D;
        for (DiscountPriceItem item : priceDetailDTO.getDiscountPriceDetail()) {
            if (item.getPromotionTypeEnum() == null || item.getDiscountPrice() == null) {
                continue;
            }
            switch (item.getPromotionTypeEnum()) {
                case FLASH_DISCOUNT -> flash = CurrencyUtil.add(flash, item.getDiscountPrice());
                case NTH_ITEM_DISCOUNT -> nth = CurrencyUtil.add(nth, item.getDiscountPrice());
                default -> { }
            }
        }
        this.setFlashDiscountSubsidy(flash);
        this.setNthItemSubsidy(nth);
    }
}