package cn.lili.modules.order.order.service;

import cn.lili.modules.order.order.entity.dos.Order;

/**
 * 订单价格
 *
 * @author Chopper
 * @since 2020/11/17 7:36 下午
 */
public interface OrderPriceService {

    /**
     * 价格修改
     * 日志功能内部实现
     *
     * @param orderSn    订单编号
     * @param orderPrice 订单价格
     * @return 订单
     */
    Order updatePrice(String orderSn, Double orderPrice);

    /**
     * 管理员订单付款
     *
     * @param orderSn 订单编号
     */
    void adminPayOrder(String orderSn);
}
