package cn.lili.modules.order.cart.entity.dto;

import cn.lili.modules.member.entity.dos.MemberAddress;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.order.cart.entity.enums.SuperpositionPromotionEnum;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.cart.entity.vo.PriceDetailVO;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.order.order.entity.vo.OrderVO;
import cn.lili.modules.order.order.entity.vo.ReceiptVO;
import cn.lili.modules.promotion.entity.dos.MemberCoupon;
import cn.lili.modules.promotion.entity.vos.GiftCardCashMemberCardVO;
import cn.lili.modules.promotion.entity.vos.MemberCouponVO;
import cn.lili.modules.store.entity.dos.StoreAddress;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * 购物车视图
 *
 * @author Chopper
 * @since 2020-03-25 2:30 下午
 */
@Data
public class TradeDTO implements Serializable {

    private static final long serialVersionUID = -3137165707807057810L;

    @Schema(description = "sn")
    private String sn;

    @Schema(description = "是否为其他订单下的订单，如果是则为依赖订单的sn，否则为空")
    private String parentOrderSn;

    @Schema(description = "购物车列表")
    private List<CartVO> cartList;

    @Schema(description = "整笔交易中所有的规格商品")
    private List<CartSkuVO> skuList;

    @Schema(description = "购物车车计算后的总价")
    private PriceDetailVO priceDetailVO;

    @Schema(description = "购物车车计算后的总价")
    private PriceDetailDTO priceDetailDTO;

    @Schema(description = "发票信息")
    private ReceiptVO receiptVO;

    @Schema(description = "是否需要发票")
    private Boolean needReceipt;


    @Schema(description = "不支持配送方式")
    private List<CartSkuVO> notSupportFreight;

    /**
     * 购物车类型
     */
    private CartTypeEnum cartTypeEnum;
    /**
     * 店铺备注
     */
    private List<StoreRemarkDTO> storeRemark;

    /**
     * sku促销连线 包含满优惠
     * <p>
     * KEY值为 sku_id+"_"+SuperpositionPromotionEnum
     * VALUE值为 对应的活动ID
     *
     * @see SuperpositionPromotionEnum
     */
    private Map<String, String> skuPromotionDetail;

    /**
     * 使用平台优惠券，一笔订单只能使用一个平台优惠券
     */
    private MemberCouponDTO platformCoupon;

    /**
     * key 为商家id
     * value 为商家优惠券
     * 店铺优惠券
     */
    private Map<String, MemberCouponDTO> storeCoupons;

    /**
     * 可用优惠券列表
     */
    private List<MemberCoupon> canUseCoupons;

    /**
     * 可用礼品卡列表
     */
    private List<GiftCardCashMemberCardVO> canUseGiftCards;

    /**
     * 无法使用优惠券无法使用的原因
     */
    private List<MemberCouponVO> cantUseCoupons;

    /**
     * 收货地址
     */
    private MemberAddress memberAddress;

    /**
     * 自提地址
     */
    private StoreAddress storeAddress;

    /**
     * 客户端类型
     */
    private String clientType;

    /**
     * 买家名称
     */
    private String memberName;

    /**
     * 买家id
     */
    private String memberId;

    /**
     * 分销商id
     */
    private String distributionId;

    /**
     * 订单vo
     */
    private List<OrderVO> orderVO;

    @Schema(description = "礼品卡抵扣总金额")
    private BigDecimal giftCardDeductTotal;

    @Schema(description = "现金实付总金额（订单差额）")
    private BigDecimal cashPayTotal;

    @Schema(description = "礼品卡抵扣明细")
    private List<cn.lili.modules.promotion.entity.vos.GiftCardCashDeductPreviewVO.DeductItem> giftCardDeductItems;

    @Schema(description = "已选礼品卡ID列表")
    private List<String> selectedGiftCardIds;

    public TradeDTO(CartTypeEnum cartTypeEnum) {
        this.cartTypeEnum = cartTypeEnum;

        this.skuList = new ArrayList<>();
        this.cartList = new ArrayList<>();
        this.skuPromotionDetail = new HashMap<>();
        this.storeCoupons = new HashMap<>();
        this.priceDetailDTO = new PriceDetailDTO();
        this.cantUseCoupons = new ArrayList<>();
        this.canUseCoupons = new ArrayList<>();
        this.canUseGiftCards = new ArrayList<>();
        this.needReceipt = false;
        this.giftCardDeductTotal = BigDecimal.ZERO;
        this.cashPayTotal = BigDecimal.ZERO;
        this.giftCardDeductItems = new ArrayList<>();
        this.selectedGiftCardIds = new ArrayList<>();
    }

    public TradeDTO() {
        this(CartTypeEnum.CART);
    }

    /**
     * 过滤购物车中已选择的sku
     *
     * @return
     */
    public List<CartSkuVO> getCheckedSkuList() {
        if (skuList != null && !skuList.isEmpty()) {
            return skuList.stream().filter(CartSkuVO::getChecked).collect(Collectors.toList());
        }
        return skuList;
    }

    public void removeCoupon() {
        this.canUseCoupons = new ArrayList<>();
        this.cantUseCoupons = new ArrayList<>();
    }
}
