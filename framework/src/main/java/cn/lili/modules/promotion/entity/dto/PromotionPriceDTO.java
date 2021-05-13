package cn.lili.modules.promotion.entity.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.List;

/**
 * 促销价格数据传输对象
 *
 * @author paulG
 * @date 2020/11/17
 **/
@Data
public class PromotionPriceDTO {

    /**
     * 实际成交价格合计
     */
    @ApiModelProperty(value = "商品原价格合计")
    private Double totalOriginPrice;

    /**
     * 总需支付积分合计
     */
    @ApiModelProperty(value = "总需支付积分合计")
    private Double totalPoints;

    /**
     * 总优惠价格合计
     */
    @ApiModelProperty(value = "总优惠价格合计")
    private Double totalDiscountPrice;

    /**
     * 优惠券合计
     */
    @ApiModelProperty(value = "优惠券合计")
    private Double totalCouponPrice;

    /**
     * 最终结算金额 = totalOriginPrice - totalDiscountPrice - totalCouponPrice
     */
    @ApiModelProperty(value = "最终结算金额")
    private Double totalFinalePrice;

    /**
     * 店铺促销计算集合
     */
    @ApiModelProperty(value = "店铺促销计算集合")
    private List<StorePromotionPriceDTO> storePromotionPriceList;

    /**
     * 参与的促销活动
     */
    @ApiModelProperty(value = "参与的促销活动")
    private List<BasePromotion> joinPromotion;

}
