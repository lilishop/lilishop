package cn.lili.modules.order.order.entity.dos;

import cn.hutool.core.text.CharSequenceUtil;
import com.alibaba.fastjson2.JSON;
import cn.lili.common.enums.ClientTypeEnum;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.security.sensitive.Sensitive;
import cn.lili.common.security.sensitive.enums.SensitiveStrategy;
import cn.lili.common.utils.BeanUtil;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.order.cart.entity.dto.MemberCouponDTO;
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
import io.swagger.v3.oas.annotations.media.Schema;
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
@Schema(description = "订单")
@NoArgsConstructor
public class Order extends BaseEntity {


    private static final long serialVersionUID = 2233811628066468683L;
    @Schema(description = "订单编号")
    private String sn;

    @Schema(description = "交易编号 关联Trade")
    private String tradeSn;

    @Schema(description = "店铺ID")
    private String storeId;

    @Schema(description = "店铺名称")
    private String storeName;

    @Schema(description = "客户ID")
    private String memberId;

    @Schema(description = "用户名")
    @Sensitive(strategy = SensitiveStrategy.PHONE)
    private String memberName;

    /**
     * @see OrderStatusEnum
     */
    @Schema(description = "订单状态")
    private String orderStatus;

    /**
     * @see PayStatusEnum
     */
    @Schema(description = "付款状态")
    private String payStatus;
    /**
     * @see DeliverStatusEnum
     */
    @Schema(description = "货运状态")
    private String deliverStatus;

    @Schema(description = "第三方付款流水号")
    private String receivableNo;

    /**
     * @see PaymentMethodEnum
     */
    @Schema(description = "支付方式")
    private String paymentMethod;

    @Schema(description = "抵扣方式：GIFT_CARD-礼品卡抵扣")
    private String deductionMethod;

    @Schema(description = "支付时间")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date paymentTime;

    @Schema(description = "收件人姓名")
    private String consigneeName;

    @Schema(description = "收件人手机")
    private String consigneeMobile;

    /**
     * @see DeliveryMethodEnum
     */
    @Schema(description = "配送方式")
    private String deliveryMethod;

    @Schema(description = "地址名称， '，'分割")
    private String consigneeAddressPath;

    @Schema(description = "地址id，'，'分割 ")
    private String consigneeAddressIdPath;

    @Schema(description = "详细地址")
    private String consigneeDetail;

    @Schema(description = "总价格")
    private Double flowPrice;

    @Schema(description = "商品价格")
    private Double goodsPrice;

    @Schema(description = "运费")
    private Double freightPrice;

    @Schema(description = "优惠的金额")
    private Double discountPrice;

    @Schema(description = "修改价格")
    private Double updatePrice;

    @Schema(description = "发货单号")
    private String logisticsNo;

    @Schema(description = "物流公司CODE")
    private String logisticsCode;

    @Schema(description = "物流公司名称")
    private String logisticsName;

    @Schema(description = "订单商品总重量")
    private Double weight;

    @Schema(description = "商品数量")
    private Integer goodsNum;

    @Schema(description = "买家订单备注")
    private String remark;

    @Schema(description = "卖家订单备注")
    private String sellerRemark;

    @Schema(description = "订单取消原因")
    private String cancelReason;

    @Schema(description = "完成时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date completeTime;

    @Schema(description = "送货时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date logisticsTime;

    @Schema(description = "支付方式返回的交易号")
    private String payOrderNo;

    /**
     * @see ClientTypeEnum
     */
    @Schema(description = "订单来源")
    private String clientType;

    @Schema(description = "是否需要发票")
    private Boolean needReceipt;

    @Schema(description = "是否为其他订单下的订单，如果是则为依赖订单的sn，否则为空")
    private String parentOrderSn = "";

    @Schema(description = "是否为某订单类型的订单，如果是则为订单类型的id，否则为空")
    private String promotionId;

    /**
     * @see OrderTypeEnum
     */
    @Schema(description = "订单类型")
    private String orderType;

    /**
     * @see OrderPromotionTypeEnum
     */
    @Schema(description = "订单促销类型")
    private String orderPromotionType;

    @Schema(description = "价格价格详情")
    private String priceDetail;

    @Schema(description = "订单是否支持原路退回")
    private Boolean canReturn;

    @Schema(description = "提货码")
    private String verificationCode;

    @Schema(description = "分销员ID")
    private String distributionId;

    @Schema(description = "使用的店铺客户优惠券id(,区分)")
    private String useStoreMemberCouponIds;

    @Schema(description = "使用的平台客户优惠券id")
    private String usePlatformMemberCouponId;

    @Schema(description = "qrCode  实物为提货码  虚拟货物为账号")
    private String qrCode;

    @Schema(description = "自提点地址")
    private String storeAddressPath;

    @Schema(description = "自提点电话")
    private String storeAddressMobile;

    @Schema(description = "自提点地址经纬度")
    private String storeAddressCenter;

    @Schema(description = "直播间Id")
    private String liveRoomId;

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
        this.setFreightPrice(cartVO.getPriceDetailDTO().getFreightPrice());
        this.applyECouponPersistNormalization();
        //客户收件信息
        if (!OrderTypeEnum.E_COUPON.name().equals(this.getOrderType())
                && tradeDTO.getMemberAddress() != null
                && DeliveryMethodEnum.LOGISTICS.name().equals(cartVO.getDeliveryMethod())) {
            this.setConsigneeAddressIdPath(tradeDTO.getMemberAddress().getConsigneeAddressIdPath());
            this.setConsigneeAddressPath(tradeDTO.getMemberAddress().getConsigneeAddressPath());
            this.setConsigneeDetail(tradeDTO.getMemberAddress().getDetail());
            this.setConsigneeMobile(tradeDTO.getMemberAddress().getMobile());
            this.setConsigneeName(tradeDTO.getMemberAddress().getName());
        }
        //自提点信息
        if (tradeDTO.getStoreAddress() != null && DeliveryMethodEnum.SELF_PICK_UP.name().equals(cartVO.getDeliveryMethod())) {
            this.setStoreAddressPath(tradeDTO.getStoreAddress().getAddress());
            this.setStoreAddressMobile(tradeDTO.getStoreAddress().getMobile());
            this.setStoreAddressCenter(tradeDTO.getStoreAddress().getCenter());
        }
        //平台优惠券判定
        if (tradeDTO.getPlatformCoupon() != null) {
            this.setUsePlatformMemberCouponId(tradeDTO.getPlatformCoupon().getMemberCoupon().getId());
        }
        //店铺优惠券判定
        if (tradeDTO.getStoreCoupons() != null && !tradeDTO.getStoreCoupons().isEmpty()) {
            StringBuilder storeCouponIds = new StringBuilder();
            for (MemberCouponDTO value : tradeDTO.getStoreCoupons().values()) {
                storeCouponIds.append(value.getMemberCoupon().getId()).append(",");
            }
            this.setUseStoreMemberCouponIds(storeCouponIds.toString());
        }

    }


    /**
     * E_COUPON 订单落库归一化：虚拟发货、运费 0、清除收件/自提/物流快照。
     * 供 {@link #Order(CartVO, TradeDTO)} 与满赠 G 子单等 bypass 构造器的创建路径共用。
     */
    public void applyECouponPersistNormalization() {
        if (!OrderTypeEnum.E_COUPON.name().equals(this.getOrderType())) {
            return;
        }
        this.setDeliveryMethod(DeliveryMethodEnum.VIRTUAL.name());
        this.setFreightPrice(0D);
        this.setConsigneeName(null);
        this.setConsigneeMobile(null);
        this.setConsigneeAddressPath(null);
        this.setConsigneeAddressIdPath(null);
        this.setConsigneeDetail(null);
        this.setStoreAddressPath(null);
        this.setStoreAddressMobile(null);
        this.setStoreAddressCenter(null);
        this.setLogisticsNo(null);
        this.setLogisticsCode(null);
        this.setLogisticsName(null);
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
            String goodsType = cartVO.getCheckedSkuList().get(0).getGoodsSku().getGoodsType();
            // 立即购买/购物车链路：E_COUPON 与 NORMAL 区分 orderType（非 VIRTUAL 核销）
            if (GoodsTypeEnum.E_COUPON.name().equals(goodsType)) {
                this.setOrderType(OrderTypeEnum.E_COUPON.name());
            } else {
                this.setOrderType(OrderTypeEnum.NORMAL.name());
            }
            this.setOrderPromotionType(OrderPromotionTypeEnum.NORMAL.name());
        } else if (tradeDTO.getCartTypeEnum().equals(CartTypeEnum.VIRTUAL)) {
            this.setOrderType(OrderTypeEnum.VIRTUAL.name());
            this.setOrderPromotionType(OrderPromotionTypeEnum.NORMAL.name());
        } else {
            //促销订单（拼团、积分、砍价）
            String goodsType = cartVO.getCheckedSkuList().get(0).getGoodsSku().getGoodsType();
            if (GoodsTypeEnum.E_COUPON.name().equals(goodsType)) {
                this.setOrderType(OrderTypeEnum.E_COUPON.name());
            } else if (CharSequenceUtil.isEmpty(goodsType) || goodsType.equals(GoodsTypeEnum.PHYSICAL_GOODS.name())) {
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
            return JSON.parseObject(priceDetail, PriceDetailDTO.class);
        } catch (Exception e) {
            return null;
        }
    }

    public void setPriceDetailDTO(PriceDetailDTO priceDetail) {
        this.priceDetail = JSON.toJSONString(priceDetail);
    }


}