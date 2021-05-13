package cn.lili.modules.order.order.service;

import cn.lili.modules.order.order.entity.dos.Order;

/**
 * @author liushuai(liushuai711 @ gmail.com)
 * @version v4.1
 * @Description:
 * @since 2021/4/28 3:47 下午
 */
public interface OrderPriceService {

    /**
     * 价格修改
     * 日志功能内部实现
     *
     * @param orderSn    订单编号
     * @param orderPrice 订单价格
     */
    Order updatePrice(String orderSn, Double orderPrice);

    /**
     * 管理员订单付款
     *
     * @param orderSn 订单编号
     */
    void adminPayOrder(String orderSn);
}
