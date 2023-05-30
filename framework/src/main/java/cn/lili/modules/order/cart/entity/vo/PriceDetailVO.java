package cn.lili.modules.order.cart.entity.vo;

import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;

/**
 * 订单价格详情
 *
 * @author Chopper
 * @since 2020-04-01 10:42 上午
 */
@Data
public class PriceDetailVO implements Serializable {

    private static final long serialVersionUID = -960537582096338500L;

    @ApiModelProperty(value = "商品原价")
    private Double originalPrice;

    @ApiModelProperty(value = "配送费")
    private Double freight;

    @ApiModelProperty(value = "优惠金额")
    private Double discountPrice;

    @ApiModelProperty(value = "支付积分")
    private Long payPoint;

    @ApiModelProperty(value = "最终成交金额")
    private Double finalePrice;


    /**
     * 构造器，初始化默认值
     */
    public PriceDetailVO(PriceDetailDTO dto) {
        this.freight = dto.getFreightPrice();
        this.finalePrice = dto.getFlowPrice();
        this.discountPrice = dto.getDiscountPrice();
        this.payPoint = dto.getPayPoint();
        this.originalPrice = dto.getGoodsPrice();
    }

    public PriceDetailVO(){

    }
}
