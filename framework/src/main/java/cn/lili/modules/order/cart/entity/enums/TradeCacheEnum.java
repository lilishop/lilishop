package cn.lili.modules.order.cart.entity.enums;

/**
 * 交易缓存枚举
 *
 * @author Chopper
 * @date 2020-03-25 2:30 下午
 */
public enum TradeCacheEnum {

    //================交易=================

    /**
     * 拼团
     */
    PINTUAN,
    /**
     * 购物车原始数据
     */
    CART_DATA,
    /**
     * 立即购买购物车原始数据
     */
    BUY_NOW_CART_DATA;


    public String getPrefix() {
        return "{" + this.name() + "}_";
    }
}
