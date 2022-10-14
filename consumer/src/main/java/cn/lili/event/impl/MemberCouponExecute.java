package cn.lili.event.impl;

import cn.hutool.core.collection.ListUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.event.AfterSaleStatusChangeEvent;
import cn.lili.event.OrderStatusChangeEvent;
import cn.lili.modules.order.aftersale.entity.dos.AfterSale;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.trade.entity.enums.AfterSaleStatusEnum;
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
public class MemberCouponExecute implements OrderStatusChangeEvent, AfterSaleStatusChangeEvent {

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
            this.refundCoupon(orderMessage.getOrderSn());
        }
    }


    @Override
    public void afterSaleStatusChange(AfterSale afterSale) {
        // 售后完成返还优惠券
        if (afterSale.getServiceStatus().equals(AfterSaleStatusEnum.COMPLETE.name())) {
            this.refundCoupon(afterSale.getOrderSn());
        }
    }

    /**
     * 退款返还优惠券
     * @param orderSn 订单编号
     */
    private void refundCoupon(String orderSn) {
        Order order = orderService.getBySn(orderSn);
        if (CharSequenceUtil.isNotEmpty(order.getUseStoreMemberCouponIds())) {
            memberCouponService.recoveryMemberCoupon(ListUtil.toList(order.getUseStoreMemberCouponIds().split(",")));
        }
        if (CharSequenceUtil.isNotEmpty(order.getUsePlatformMemberCouponId())) {
            memberCouponService.recoveryMemberCoupon(ListUtil.toList(order.getUsePlatformMemberCouponId().split(",")));
        }
    }
}
