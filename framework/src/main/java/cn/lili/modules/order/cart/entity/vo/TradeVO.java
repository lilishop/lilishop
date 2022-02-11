package cn.lili.modules.order.cart.entity.vo;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;
import java.util.List;

/**
 * 整比交易对象
 *
 * @author Chopper
 * @since 2020-04-01 10:42 上午
 */
@Data
public class TradeVO implements Serializable {

    private static final long serialVersionUID = -4563542542090139404L;

    /**
     * 购物车列表
     */
    @ApiModelProperty(value = "购物车列表")
    private List<CartVO> cartList;

    /**
     * 购物车计算后的总价
     */
    @ApiModelProperty(value = "购物车车计算后的总价")
    private PriceDetailVO priceDetailVO;

    public TradeVO(List<CartVO> cartList, PriceDetailVO priceDetailVO) {
        this.cartList = cartList;
        this.priceDetailVO = priceDetailVO;
    }


}
