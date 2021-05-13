package cn.lili.modules.promotion.entity.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.List;

/**
 * 店铺促销计算数据传输对象
 *
 * @author paulG
 * @date 2020/11/17
 **/
@Data
public class StorePromotionPriceDTO {

    /**
     * 店铺ID
     */
    @ApiModelProperty(value = "店铺ID")
    private String storeId;

    /**
     * 是否免运费
     */
    @ApiModelProperty(value = "是否免运费")
    private Boolean isFreeFreight;

    /**
     * 店铺商品数量合计
     */
    @ApiModelProperty(value = "店铺商品数量合计")
    private Integer totalNum;

    /**
     * 店铺商品重量合计
     */
    @ApiModelProperty(value = "店铺商品重量合计")
    private Double totalWeight;

    /**
     * 店铺商品原价格合计
     */
    @ApiModelProperty(value = "店铺商品原价格合计")
    private Double totalOriginPrice;

    /**
     * 店铺商品需支付积分合计
     */
    @ApiModelProperty(value = "店铺商品需支付积分合计")
    private Double totalPoints;

    /**
     * 店铺商品最终成交的总金额
     */
    @ApiModelProperty(value = "店铺商品最终成交的总金额")
    private Double totalFinalePrice;

    /**
     * 店铺参与促销商品价格合计
     */
    @ApiModelProperty(value = "店铺参与满优惠商品价格合计")
    private Double totalJoinDiscountPrice;

    /**
     * 店铺未参与促销商品价格合计
     */
    @ApiModelProperty(value = "店铺未参与满优惠商品价格合计")
    private Double totalNotJoinDiscountPrice;

    /**
     * 店铺商品优惠价格合计
     */
    @ApiModelProperty(value = "店铺商品优惠价格合计")
    private Double totalDiscountPrice;

    /**
     * 优惠券合计
     */
    @ApiModelProperty(value = "优惠券合计")
    private Double totalCouponPrice;

    /**
     * 店铺商品促销信息集合
     */
    @ApiModelProperty(value = "店铺商品促销信息集合")
    private List<GoodsSkuPromotionPriceDTO> goodsSkuPromotionPriceList;

    /**
     * 参与的促销活动
     */
    @ApiModelProperty(value = "参与的促销活动")
    private List<BasePromotion> joinPromotion;

    //=========distribution==========

    @ApiModelProperty(value = "1级单品分销返现支出")
    private Double distributionCommission1;

    @ApiModelProperty(value = "2级单品分销返现支出")
    private Double distributionCommission2;

    @ApiModelProperty(value = "平台收取交易佣金")
    private Double platFormCommission;

    //=========end distribution==========

}
