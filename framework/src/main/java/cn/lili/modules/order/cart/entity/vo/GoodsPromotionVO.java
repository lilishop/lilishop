package cn.lili.modules.order.cart.entity.vo;

import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;
import java.util.Date;

/**
 * 商品促销VO
 *
 * @author Chopper
 * @since 2020-04-01 10:42 上午
 */
@Data
@ApiModel(description = "购物车中")
public class GoodsPromotionVO implements Serializable {


    private static final long serialVersionUID = 1622051257060817414L;
    @ApiModelProperty(value = "活动开始时间")
    private Date startTime;

    @ApiModelProperty(value = "活动结束时间")
    private Date endTime;

    @ApiModelProperty(value = "活动id")
    private String promotionId;

    /**
     * @see PromotionTypeEnum
     */
    @ApiModelProperty(value = "活动工具类型")
    private String promotionType;

    @ApiModelProperty(value = "活动名称")
    private String title;


    @ApiModelProperty(value = "限购数量")
    private Integer limitNum;

    public GoodsPromotionVO(PromotionGoods promotionGoods) {
        this.startTime = promotionGoods.getStartTime();
        this.endTime = promotionGoods.getEndTime();
        this.promotionId = promotionGoods.getPromotionId();
        this.setPromotionType(promotionGoods.getPromotionType());
        this.setLimitNum(promotionGoods.getLimitNum());
    }

    public GoodsPromotionVO() {

    }
}
