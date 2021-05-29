package cn.lili.event.impl;

import cn.lili.common.utils.CommonUtil;
import cn.lili.event.OrderStatusChangeEvent;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.service.OrderService;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 虚拟商品
 * @author Bulbasaur
 * @date: 2021/5/29 9:17 上午
 *
 */
@Component
public class VerificationOrderExecute implements OrderStatusChangeEvent {

    @Autowired
    private OrderService orderService;
    @Override
    public void orderChange(OrderMessage orderMessage) {
        //订单状态为待核验，添加订单添加核验码
        if(orderMessage.getNewStatus().equals(OrderStatusEnum.TAKE)) {
                String code = CommonUtil.getRandomNum();
                orderService.update(new LambdaUpdateWrapper<Order>()
                        .set(Order::getVerificationCode, code)
                        .eq(Order::getSn, orderMessage.getOrderSn()));
            }
    }
}
