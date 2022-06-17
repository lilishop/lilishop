package cn.lili.modules.order.cart.entity.vo;

import cn.lili.modules.distribution.entity.dos.DistributionGoods;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.promotion.tools.PromotionTools;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Map;

/**
 * 购物车中的产品
 *
 * @author Chopper
 * @since 2020-03-24 10:33 上午
 */
@Data
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class CartSkuVO extends CartBase implements Serializable {


    private static final long serialVersionUID = -894598033321906974L;


    private String sn;
    /**
     * 对应的sku DO
     */
    private GoodsSku goodsSku;

    /**
     * 分销描述
     */
    private DistributionGoods distributionGoods;

    @ApiModelProperty(value = "购买数量")
    private Integer num;

    @ApiModelProperty(value = "购买时的成交价")
    private Double purchasePrice;

    @ApiModelProperty(value = "小记")
    private Double subTotal;

    @ApiModelProperty(value = "小记")
    private Double utilPrice;
    /**
     * 是否选中，要去结算 0:未选中 1:已选中，默认
     */
    @ApiModelProperty(value = "是否选中，要去结算")
    private Boolean checked;

    @ApiModelProperty(value = "是否免运费")
    private Boolean isFreeFreight;

    @ApiModelProperty(value = "是否失效 ")
    private Boolean invalid;

    @ApiModelProperty(value = "购物车商品错误消息")
    private String errorMessage;

    @ApiModelProperty(value = "是否可配送")
    private Boolean isShip;

    @ApiModelProperty(value = "拼团id 如果是拼团购买 此值为拼团活动id，" +
            "当pintuanId为空，则表示普通购买（或者拼团商品，单独购买）")
    private String pintuanId;

    @ApiModelProperty(value = "砍价ID")
    private String kanjiaId;

    @ApiModelProperty(value = "积分兑换ID")
    private String pointsId;

    @ApiModelProperty(value = "积分购买 积分数量")
    private Long point;

    @ApiModelProperty("商品促销活动集合，key 为 促销活动类型，value 为 促销活动实体信息 ")
    private Map<String, Object> promotionMap;

    /**
     * @see CartTypeEnum
     */
    @ApiModelProperty(value = "购物车类型")
    private CartTypeEnum cartType;

    /**
     * 在构造器里初始化促销列表，规格列表
     */
    public CartSkuVO(GoodsSku goodsSku) {
        this.goodsSku = goodsSku;
        this.checked = true;
        this.invalid = false;
        //默认时间为0，让系统为此商品更新缓存
        this.errorMessage = "";
        this.isShip = true;
        this.purchasePrice = goodsSku.getPromotionFlag() != null && goodsSku.getPromotionFlag() ? goodsSku.getPromotionPrice() : goodsSku.getPrice();
        this.isFreeFreight = false;
        this.utilPrice = goodsSku.getPromotionFlag() != null && goodsSku.getPromotionFlag() ? goodsSku.getPromotionPrice() : goodsSku.getPrice();
        this.setStoreId(goodsSku.getStoreId());
        this.setStoreName(goodsSku.getStoreName());
    }

    /**
     * 在构造器里初始化促销列表，规格列表
     */
    public CartSkuVO(GoodsSku goodsSku, Map<String, Object> promotionMap) {
        this(goodsSku);
        if (promotionMap != null && !promotionMap.isEmpty()) {
            this.promotionMap = promotionMap;
        }
    }

    public Map<String, Object> getPromotionMap() {
        return PromotionTools.filterInvalidPromotionsMap(this.promotionMap);
    }

    public Map<String, Object> getNotFilterPromotionMap() {
        return this.promotionMap;
    }
}
