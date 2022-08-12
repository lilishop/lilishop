package cn.lili.event.impl;

import cn.hutool.core.collection.ListUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.event.OrderStatusChangeEvent;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.promotion.service.MemberCouponService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * 会员优惠券执行类
 *
 * @author paulG
 * @since 2022/8/12
 **/
@Service
public class MemberCouponExecute implements OrderStatusChangeEvent {

    /**
     * 订单
     */
    @Autowired
    private OrderService orderService;

    @Autowired
    private MemberCouponService memberCouponService;

    @Override
    public void orderChange(OrderMessage orderMessage) {
        // 订单取消返还优惠券
        if (orderMessage.getNewStatus() == OrderStatusEnum.CANCELLED) {
            Order order = orderService.getBySn(orderMessage.getOrderSn());
            if (CharSequenceUtil.isNotEmpty(order.getUseStoreMemberCouponIds())) {
                memberCouponService.recoveryMemberCoupon(ListUtil.toList(order.getUseStoreMemberCouponIds().split(",")));
            } else if (CharSequenceUtil.isNotEmpty(order.getUsePlatformMemberCouponId())) {
                memberCouponService.recoveryMemberCoupon(ListUtil.toList(order.getUsePlatformMemberCouponId().split(",")));
            }
        }
    }


}
