package cn.lili.modules.order.order.entity.dos;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ClientTypeEnum;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.utils.BeanUtil;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.order.cart.entity.enums.DeliveryMethodEnum;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.order.order.entity.enums.*;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;
import java.util.Optional;

/**
 * 订单
 *
 * @author Chopper
 * @since 2020/11/17 7:30 下午
 */
@EqualsAndHashCode(callSuper = true)
@Data
@TableName("li_order")
@ApiModel(value = "订单")
@NoArgsConstructor
public class Order extends BaseEntity {


    private static final long serialVersionUID = 2233811628066468683L;
    @ApiModelProperty("订单编号")
    private String sn;

    @ApiModelProperty("交易编号 关联Trade")
    private String tradeSn;

    @ApiModelProperty(value = "店铺ID")
    private String storeId;

    @ApiModelProperty(value = "店铺名称")
    private String storeName;

    @ApiModelProperty(value = "会员ID")
    private String memberId;

    @ApiModelProperty(value = "用户名")
    private String memberName;

    /**
     * @see OrderStatusEnum
     */
    @ApiModelProperty(value = "订单状态")
    private String orderStatus;

    /**
     * @see PayStatusEnum
     */
    @ApiModelProperty(value = "付款状态")
    private String payStatus;
    /**
     * @see DeliverStatusEnum
     */
    @ApiModelProperty(value = "货运状态")
    private String deliverStatus;

    @ApiModelProperty(value = "第三方付款流水号")
    private String receivableNo;

    /**
     * @see  PaymentMethodEnum
     */
    @ApiModelProperty(value = "支付方式")
    private String paymentMethod;

    @ApiModelProperty(value = "支付时间")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date paymentTime;

    @ApiModelProperty(value = "收件人姓名")
    private String consigneeName;

    @ApiModelProperty(value = "收件人手机")
    private String consigneeMobile;

    /**
     * @see DeliveryMethodEnum
     */
    @ApiModelProperty(value = "配送方式")
    private String deliveryMethod;

    @ApiModelProperty(value = "地址名称， '，'分割")
    private String consigneeAddressPath;

    @ApiModelProperty(value = "地址id，'，'分割 ")
    private String consigneeAddressIdPath;

    @ApiModelProperty(value = "详细地址")
    private String consigneeDetail;

    @ApiModelProperty(value = "总价格")
    private Double flowPrice;

    @ApiModelProperty(value = "商品价格")
    private Double goodsPrice;

    @ApiModelProperty(value = "运费")
    private Double freightPrice;

    @ApiModelProperty(value = "优惠的金额")
    private Double discountPrice;

    @ApiModelProperty(value = "修改价格")
    private Double updatePrice;

    @ApiModelProperty(value = "发货单号")
    private String logisticsNo;

    @ApiModelProperty(value = "物流公司CODE")
    private String logisticsCode;

    @ApiModelProperty(value = "物流公司名称")
    private String logisticsName;

    @ApiModelProperty(value = "订单商品总重量")
    private Double weight;

    @ApiModelProperty(value = "商品数量")
    private Integer goodsNum;

    @ApiModelProperty(value = "买家订单备注")
    private String remark;

    @ApiModelProperty(value = "订单取消原因")
    private String cancelReason;

    @ApiModelProperty(value = "完成时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date completeTime;

    @ApiModelProperty(value = "送货时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date logisticsTime;

    @ApiModelProperty(value = "支付方式返回的交易号")
    private String payOrderNo;

    /**
     * @see ClientTypeEnum
     */
    @ApiModelProperty(value = "订单来源")
    private String clientType;

    @ApiModelProperty(value = "是否需要发票")
    private Boolean needReceipt;

    @ApiModelProperty(value = "是否为其他订单下的订单，如果是则为依赖订单的sn，否则为空")
    private String parentOrderSn = "";

    @ApiModelProperty(value = "是否为某订单类型的订单，如果是则为订单类型的id，否则为空")
    private String promotionId;

    /**
     * @see OrderTypeEnum
     */
    @ApiModelProperty(value = "订单类型")
    private String orderType;

    /**
     * @see OrderPromotionTypeEnum
     */
    @ApiModelProperty(value = "订单促销类型")
    private String orderPromotionType;

    @ApiModelProperty(value = "价格价格详情")
    private String priceDetail;

    @ApiModelProperty(value = "订单是否支持原路退回")
    private Boolean canReturn;

    @ApiModelProperty(value = "提货码")
    private String verificationCode;

    @ApiModelProperty(value = "分销员ID")
    private String distributionId;

    @ApiModelProperty(value = "使用的店铺会员优惠券id(,区分)")
    private String useStoreMemberCouponIds;

    @ApiModelProperty(value = "使用的平台会员优惠券id")
    private String usePlatformMemberCouponId;

    /**
     * 构建订单
     *
     * @param cartVO   购物车VO
     * @param tradeDTO 交易DTO
     */
    public Order(CartVO cartVO, TradeDTO tradeDTO) {
        String oldId = this.getId();
        BeanUtil.copyProperties(tradeDTO, this);
        BeanUtil.copyProperties(cartVO.getPriceDetailDTO(), this);
        BeanUtil.copyProperties(cartVO, this);
        //填写订单类型
        this.setTradeType(cartVO, tradeDTO);
        setId(oldId);

        //设置默认支付状态
        this.setOrderStatus(OrderStatusEnum.UNPAID.name());
        this.setPayStatus(PayStatusEnum.UNPAID.name());
        this.setDeliverStatus(DeliverStatusEnum.UNDELIVERED.name());
        this.setTradeSn(tradeDTO.getSn());
        this.setRemark(cartVO.getRemark());
        this.setFreightPrice(tradeDTO.getPriceDetailDTO().getFreightPrice());
        //会员收件信息
        this.setConsigneeAddressIdPath(tradeDTO.getMemberAddress().getConsigneeAddressIdPath());
        this.setConsigneeAddressPath(tradeDTO.getMemberAddress().getConsigneeAddressPath());
        this.setConsigneeDetail(tradeDTO.getMemberAddress().getDetail());
        this.setConsigneeMobile(tradeDTO.getMemberAddress().getMobile());
        this.setConsigneeName(tradeDTO.getMemberAddress().getName());
        //平台优惠券判定
        if (tradeDTO.getPlatformCoupon() != null) {
            this.setUsePlatformMemberCouponId(tradeDTO.getPlatformCoupon().getMemberCoupon().getId());
        }
        //店铺优惠券判定
        if (tradeDTO.getStoreCoupons() != null && !tradeDTO.getStoreCoupons().isEmpty()) {
            StringBuilder storeCouponIds = new StringBuilder();
            for (String s : tradeDTO.getStoreCoupons().keySet()) {
                storeCouponIds.append(s).append(",");
            }
            this.setUseStoreMemberCouponIds(storeCouponIds.toString());
        }

    }


    /**
     * 填写交易（订单）类型
     * 1.判断是普通、促销订单
     * 2.普通订单进行区分：实物订单、虚拟订单
     * 3.促销订单判断货物进行区分实物、虚拟商品。
     * 4.拼团订单需要填写父订单ID
     *
     * @param cartVO   购物车VO
     * @param tradeDTO 交易DTO
     */
    private void setTradeType(CartVO cartVO, TradeDTO tradeDTO) {

        //判断是否为普通订单、促销订单
        if (tradeDTO.getCartTypeEnum().equals(CartTypeEnum.CART) || tradeDTO.getCartTypeEnum().equals(CartTypeEnum.BUY_NOW)) {
            this.setOrderType(OrderTypeEnum.NORMAL.name());
            this.setOrderPromotionType(OrderPromotionTypeEnum.NORMAL.name());
        } else if (tradeDTO.getCartTypeEnum().equals(CartTypeEnum.VIRTUAL)) {
            this.setOrderType(OrderTypeEnum.VIRTUAL.name());
            this.setOrderPromotionType(OrderPromotionTypeEnum.NORMAL.name());
        } else {
            //促销订单（拼团、积分）-判断购买的是虚拟商品还是实物商品
            String goodsType = cartVO.getCheckedSkuList().get(0).getGoodsSku().getGoodsType();
            if (CharSequenceUtil.isEmpty(goodsType) || goodsType.equals(GoodsTypeEnum.PHYSICAL_GOODS.name())) {
                this.setOrderType(OrderTypeEnum.NORMAL.name());
            } else {
                this.setOrderType(OrderTypeEnum.VIRTUAL.name());
            }
            //填写订单的促销类型
            this.setOrderPromotionType(tradeDTO.getCartTypeEnum().name());

            //判断是否为拼团订单，如果为拼团订单获取拼团ID，判断是否为主订单
            if (tradeDTO.getCartTypeEnum().name().equals(PromotionTypeEnum.PINTUAN.name()) && cartVO.getCheckedSkuList().get(0).getPromotionMap() != null && !cartVO.getCheckedSkuList().get(0).getPromotionMap().isEmpty()) {
                Optional<String> pintuanPromotions = cartVO.getCheckedSkuList().get(0).getPromotionMap().keySet().stream().filter(i -> i.contains(PromotionTypeEnum.PINTUAN.name())).findFirst();
                pintuanPromotions.ifPresent(s -> promotionId = s.split("-")[1]);
            }
        }
    }


    public PriceDetailDTO getPriceDetailDTO() {

        try {
            return JSONUtil.toBean(priceDetail, PriceDetailDTO.class);
        } catch (Exception e) {
            return null;
        }
    }

    public void setPriceDetailDTO(PriceDetailDTO priceDetail) {
        this.priceDetail = JSONUtil.toJsonStr(priceDetail);
    }


}