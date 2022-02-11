package cn.lili.modules.order.cart.entity.vo;

import cn.lili.modules.order.cart.entity.enums.DeliveryMethodEnum;
import cn.lili.modules.promotion.entity.dos.MemberCoupon;
import cn.lili.modules.promotion.entity.vos.CouponVO;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;


/**
 * 购物车展示VO
 *
 * @author Chopper
 * @since 2020-03-24 10:33 上午
 */
@Data
@ApiModel(description = "购物车")
@NoArgsConstructor
public class CartVO extends CartBase implements Serializable {

    private static final long serialVersionUID = -5651775413457562422L;

    @ApiModelProperty(value = "购物车中的产品列表")
    private List<CartSkuVO> skuList;

    @ApiModelProperty(value = "sn")
    private String sn;

    @ApiModelProperty(value = "购物车页展示时，店铺内的商品是否全选状态.1为店铺商品全选状态,0位非全选")
    private Boolean checked;

    @ApiModelProperty(value = "满优惠活动")
    private FullDiscountVO fullDiscount;

    @ApiModelProperty(value = "满优惠促销的商品")
    private List<String> fullDiscountSkuIds;

    @ApiModelProperty(value = "是否满优惠")
    private Boolean isFull;

    @ApiModelProperty(value = "使用的优惠券列表")
    private List<MemberCoupon> couponList;

    @ApiModelProperty(value = "使用的优惠券列表")
    private List<CouponVO> canReceiveCoupon;

    @ApiModelProperty(value = "赠品列表")
    private List<String> giftList;

    @ApiModelProperty(value = "赠送优惠券列表")
    private List<String> giftCouponList;

    @ApiModelProperty(value = "赠送积分")
    private Integer giftPoint;

    @ApiModelProperty(value = "重量")
    private Double weight;

    @ApiModelProperty(value = "购物车商品数量")
    private Integer goodsNum;

    @ApiModelProperty(value = "购物车商品数量")
    private String remark;

    /**
     * @see DeliveryMethodEnum
     */
    @ApiModelProperty(value = "配送方式")
    private String deliveryMethod;

    @ApiModelProperty(value = "已参与的的促销活动提示，直接展示给客户")
    private String promotionNotice;

    public CartVO(CartSkuVO cartSkuVO) {
        this.setStoreId(cartSkuVO.getStoreId());
        this.setStoreName(cartSkuVO.getStoreName());
        this.setSkuList(new ArrayList<>());
        this.setCouponList(new ArrayList<>());
        this.setGiftList(new ArrayList<>());
        this.setGiftCouponList(new ArrayList<>());
        this.setCanReceiveCoupon(new ArrayList<>());
        this.setChecked(false);
        this.isFull = false;
        this.weight = 0d;
        this.giftPoint = 0;
        this.remark = "";
    }

    public void addGoodsNum(Integer goodsNum) {
        if (this.goodsNum == null) {
            this.goodsNum = goodsNum;
        } else {
            this.goodsNum += goodsNum;
        }
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

}
