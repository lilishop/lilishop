package cn.lili.modules.order.trade.service;

import cn.lili.modules.order.trade.entity.dos.OrderLog;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 订单日志业务层
 *
 * @author Chopper
 * @since 2020-02-25 14:10:16
 */
public interface OrderLogService extends IService<OrderLog> {

    /**
     * 根据订单编号获取订单日志列表
     * @param orderSn 订单编号
     * @return 订单日志列表
     */
    List<OrderLog> getOrderLog(String orderSn);
}