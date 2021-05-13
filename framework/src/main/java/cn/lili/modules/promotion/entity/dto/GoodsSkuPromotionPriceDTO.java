package cn.lili.modules.promotion.entity.dto;

import cn.lili.common.utils.CurrencyUtil;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * 促销活动商品价格数据传输对象
 *
 * @author paulG
 * @date 2020/8/20
 **/
@Data
@NoArgsConstructor
public class GoodsSkuPromotionPriceDTO implements Serializable {

    private static final long serialVersionUID = 3510264801983456306L;

    @ApiModelProperty(value = "商品SkuId")
    private String skuId;

    @ApiModelProperty(value = "店铺Id")
    private String storeId;

    @ApiModelProperty(value = "分类path")
    private String categoryPath;

    @ApiModelProperty(value = "店铺分类id")
    private String storeCategoryPath;

    @ApiModelProperty(value = "数量")
    private Integer number;

    @ApiModelProperty(value = "重量")
    private Double weight;

    @ApiModelProperty(value = "总重量")
    private Double totalWeight;

    @ApiModelProperty(value = "单个商品原价")
    private Double originalPrice;

    @ApiModelProperty(value = "商品原价总价 = 商品原价 * 数量")
    private Double totalOriginalPrice;

    @ApiModelProperty(value = "单个商品积分购买数量")
    private Double points;

    @ApiModelProperty(value = "商品购买总数量 = 单个商品积分购买数量 * 数量")
    private Double totalPoints;

    @ApiModelProperty(value = "单个优惠的所占总优惠金额比例")
    private Double discountPriceRate;

    @ApiModelProperty(value = "单个优惠的金额")
    private Double discountPrice;

    @ApiModelProperty(value = "优惠的总金额 = 单个优惠的金额 * 数量")
    private Double totalDiscountPrice;

    @ApiModelProperty(value = "单个商品最终成交金额")
    private Double finalePrice;

    @ApiModelProperty(value = "商品最终成交的总金额 = 单个商品最终成交金额 * 数量")
    private Double totalFinalePrice;

    @ApiModelProperty(value = "分配到每个商品的优惠券金额")
    private Double couponPrice;

    @ApiModelProperty(value = "促销活动ID")
    private String promotionId;

    /**
     * @see cn.lili.modules.promotion.entity.enums.PromotionTypeEnum
     */
    @ApiModelProperty(value = "促销活动类型")
    private String promotionType;

    /**
     * 店铺商品促销信息集合
     */
    @ApiModelProperty(value = "参与的促销活动")
    private List<BasePromotion> joinPromotion;

    public GoodsSkuPromotionPriceDTO(GoodsSku sku, Integer buyNum) {
        this.setOriginalPrice(sku.getPrice());
        this.setSkuId(sku.getId());
        this.setNumber(buyNum);
        this.setWeight(sku.getWeight());
        this.setTotalWeight(sku.getWeight() != null ? CurrencyUtil.mul(sku.getWeight(), buyNum) : 0);
        this.setCategoryPath(sku.getCategoryPath());
        this.setStoreCategoryPath(sku.getStoreCategoryPath());
        this.setStoreId(sku.getStoreId());
        this.setDiscountPrice(0d);
        this.setOriginalPrice(sku.getPrice());
        this.setCouponPrice(0D);
        this.setPoints(0d);
        this.setTotalPoints(0d);
        this.setFinalePrice(sku.getPrice());
        this.setJoinPromotion(new ArrayList<>());
    }
}
