package cn.lili.event.impl;

import cn.hutool.core.util.RandomUtil;
import cn.lili.event.OrderStatusChangeEvent;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.order.entity.enums.OrderComplaintStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderItemAfterSaleStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 虚拟商品
 *
 * @author Bulbasaur
 * @since 2021/5/29 9:17 上午
 */
@Component
public class VerificationOrderExecute implements OrderStatusChangeEvent {

    @Autowired
    private OrderService orderService;
    @Autowired
    private OrderItemService orderItemService;
    @Override
    public void orderChange(OrderMessage orderMessage) {
        //订单状态为待核验，添加订单添加核验码
        if (orderMessage.getNewStatus().equals(OrderStatusEnum.TAKE)) {
            //获取订单信息
            Order order = orderService.getBySn(orderMessage.getOrderSn());
            //获取随机数，判定是否存在
            String code = getCode(order.getStoreId());
            //设置订单验证码
            orderService.update(new LambdaUpdateWrapper<Order>()
                    .set(Order::getVerificationCode, code)
                    .eq(Order::getSn, orderMessage.getOrderSn()));
            //修改虚拟订单货物可以进行售后、投诉
            orderItemService.update(new LambdaUpdateWrapper<OrderItem>().eq(OrderItem::getOrderSn, orderMessage.getOrderSn())
                    .set(OrderItem::getAfterSaleStatus, OrderItemAfterSaleStatusEnum.NOT_APPLIED)
                    .set(OrderItem::getCommentStatus, OrderComplaintStatusEnum.NO_APPLY));
        }
    }

    /**
     * 获取随机数
     * 判断当前店铺下是否使用验证码，如果已使用则重新获取
     *
     * @param storeId 店铺ID
     * @return
     */
    private String getCode(String storeId) {
        //获取八位验证码
        String code = Long.toString(RandomUtil.randomLong(10000000, 99999999));

        LambdaQueryWrapper lambdaQueryWrapper = new LambdaQueryWrapper<Order>()
                .eq(Order::getVerificationCode, code)
                .eq(Order::getStoreId, storeId);
        if (orderService.getOne(lambdaQueryWrapper) == null) {
            return code;
        } else {
            return this.getCode(storeId);
        }
    }
}
