package cn.lili.modules.order.order.entity.dos;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.utils.BeanUtil;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.common.utils.SnowFlake;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.order.order.entity.enums.FlowTypeEnum;
import cn.lili.modules.order.order.entity.enums.OrderPromotionTypeEnum;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.mybatis.BaseIdEntity;
import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
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
@ApiModel(value = "商家订单流水")
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class StoreFlow extends BaseIdEntity {

    private static final long serialVersionUID = -5998757398902747939L;

    @ApiModelProperty(value = "流水编号")
    private String sn;

    @ApiModelProperty(value = "订单sn")
    private String orderSn;

    @ApiModelProperty(value = "子订单sn")
    private String orderItemSn;

    @ApiModelProperty(value = "售后SN")
    private String refundSn;

    @ApiModelProperty(value = "店铺id")
    private String storeId;

    @ApiModelProperty(value = "店铺名称 ")
    private String storeName;

    @ApiModelProperty(value = "会员id")
    private String memberId;

    @ApiModelProperty(value = "会员名称")
    private String memberName;


    @ApiModelProperty(value = "商品ID")
    private String goodsId;

    @ApiModelProperty(value = "商品名称")
    private String goodsName;

    @ApiModelProperty(value = "货品ID")
    private String skuId;

    @ApiModelProperty(value = "图片")
    private String image;

    @ApiModelProperty(value = "分类ID")
    private String categoryId;

    @ApiModelProperty(value = "规格json")
    private String specs;


    /**
     * @see FlowTypeEnum
     */
    @ApiModelProperty(value = "流水类型：PAY/REFUND 支付/退款", allowableValues = "PAY,REFUND")
    private String flowType;

    /**
     * @see cn.lili.modules.order.order.entity.enums.OrderPromotionTypeEnum
     */
    @ApiModelProperty(value = "订单促销类型")
    private String orderPromotionType;

    @ApiModelProperty(value = "积分活动商品结算价格")
    private Double pointSettlementPrice;

    @ApiModelProperty(value = "砍价活动商品结算价格")
    private Double kanjiaSettlementPrice;

    @ApiModelProperty(value = "平台优惠券 使用金额")
    private Double siteCouponPrice;

    @ApiModelProperty(value = "站点优惠券补贴比例")
    private Double siteCouponPoint;

    @ApiModelProperty(value = "站点优惠券补贴金额")
    private Double siteCouponCommission;

    @ApiModelProperty(value = "单品分销返现支出")
    private Double distributionRebate;

    @ApiModelProperty(value = "平台收取交易佣金")
    private Double commissionPrice;

    @ApiModelProperty(value = "流水金额")
    private Double finalPrice;

    @ApiModelProperty(value = "最终结算金额")
    private Double billPrice;

    @ApiModelProperty(value = "第三方交易流水号")
    private String transactionId;

    /**
     * @see PaymentMethodEnum
     */
    @ApiModelProperty(value = "支付方式名称")
    private String paymentName;

    @ApiModelProperty(value = "销售量")
    private Integer num;

    @CreatedDate
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @TableField(fill = FieldFill.INSERT)
    @ApiModelProperty(value = "创建时间", hidden = true)
    private Date createTime;


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
        //店铺流水金额=goodsPrice(商品总金额（商品原价）)+ freightPrice(配送费) - discountPrice(优惠金额) - couponPrice(优惠券金额) + updatePrice(订单修改金额)
        this.setFinalPrice(item.getPriceDetailDTO().getFlowPrice());
        //平台收取交易佣金=(flowPrice(流水金额) * platFormCommissionPoint(平台佣金比例))/100
        this.setCommissionPrice(item.getPriceDetailDTO().getPlatFormCommission());
        //单品分销返现支出
        this.setDistributionRebate(item.getPriceDetailDTO().getDistributionCommission());
        //最终结算金额=flowPrice(流水金额) - platFormCommission(平台收取交易佣金) - distributionCommission(单品分销返现支出)
        this.setBillPrice(item.getPriceDetailDTO().getBillPrice());
        //兼容为空，以及普通订单操作
        if (CharSequenceUtil.isNotEmpty(promotionType)) {
            //如果为砍价活动，填写砍价结算价
            if (promotionType.equals(OrderPromotionTypeEnum.KANJIA.name())) {
                this.setKanjiaSettlementPrice(item.getPriceDetailDTO().getSettlementPrice());
            }
            //如果为砍价活动，填写砍价结算价
            else if (promotionType.equals(OrderPromotionTypeEnum.POINTS.name())) {
                this.setPointSettlementPrice(item.getPriceDetailDTO().getSettlementPrice());
            }
        }
        //添加支付方式
        this.setPaymentName(order.getPaymentMethod());
        //添加第三方支付流水号
        this.setTransactionId(order.getReceivableNo());

    }
}