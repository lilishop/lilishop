package cn.lili.modules.order.aftersale.support;

import cn.lili.common.utils.CurrencyUtil;
import cn.lili.modules.appointment.service.AppointmentService;
import cn.lili.modules.order.aftersale.entity.dos.AfterSale;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.enums.OrderItemAfterSaleStatusEnum;
import cn.lili.modules.order.order.entity.enums.RefundStatusEnum;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.trade.entity.enums.AfterSaleStatusEnum;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 售后状态变更同步支持类，负责售后单状态流转后同步更新订单商品项的售后数量、
 * 退款金额与售后/退款状态标识，以及通知预约履约模块回补预约时段。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
@Component
public class AfterSaleOrderItemSyncSupport {

    @Autowired
    private OrderItemService orderItemService;

    @Autowired
    private OrderService orderService;

    @Autowired
    private AppointmentService appointmentService;

    public AfterSaleOrderItemSyncSupport() {
    }

    public AfterSaleOrderItemSyncSupport(OrderItemService orderItemService,
                                         OrderService orderService,
                                         AppointmentService appointmentService) {
        this.orderItemService = orderItemService;
        this.orderService = orderService;
        this.appointmentService = appointmentService;
    }

    /**
     * 根据售后单当前状态同步修改对应订单商品项的退货数量、已退金额及售后状态。
     *
     * @param afterSale 售后单
     */
    public void updateOrderItemAfterSaleStatus(AfterSale afterSale) {
        OrderItem orderItem = orderItemService.getOne(new LambdaQueryWrapper<OrderItem>()
                .eq(OrderItem::getOrderSn, afterSale.getOrderSn())
                .eq(OrderItem::getSkuId, afterSale.getSkuId()));
        AfterSaleStatusEnum afterSaleStatusEnum = AfterSaleStatusEnum.valueOf(afterSale.getServiceStatus());

        switch (afterSaleStatusEnum) {
            case APPLY:
                orderItem.setReturnGoodsNumber(orderItem.getReturnGoodsNumber() + afterSale.getNum());
                orderItem.setRefundPrice(CurrencyUtil.add(orderItem.getRefundPrice(), afterSale.getApplyRefundPrice()));
                this.updateOrderItem(orderItem, afterSale);
                break;
            case REFUSE:
            case BUYER_CANCEL:
            case SELLER_TERMINATION:
                orderItem.setReturnGoodsNumber(orderItem.getReturnGoodsNumber() - afterSale.getNum());
                orderItem.setRefundPrice(CurrencyUtil.sub(orderItem.getRefundPrice(), afterSale.getApplyRefundPrice()));
                this.updateOrderItem(orderItem, afterSale);
                break;
            default:
                break;
        }
    }

    /**
     * 预约订单售后完成时通知履约服务：部分退不关单，退尽关单并回补预约时段。
     *
     * @param afterSale 售后单
     */
    public void notifyAppointmentRefundIfNeeded(AfterSale afterSale) {
        if (!AfterSaleStatusEnum.COMPLETE.name().equals(afterSale.getServiceStatus())) {
            return;
        }
        Order order = orderService.getBySn(afterSale.getOrderSn());
        if (!appointmentService.isAppointmentOrder(order)) {
            return;
        }
        double amount = afterSale.getActualRefundPrice() == null
                ? afterSale.getApplyRefundPrice() : afterSale.getActualRefundPrice();
        appointmentService.onRefundSuccess(afterSale.getOrderSn(), amount);
    }

    /**
     * 刷新订单商品项的售后状态及退款状态字段。
     *
     * @param orderItem 订单商品项
     * @param afterSale 售后单
     */
    private void updateOrderItem(OrderItem orderItem, AfterSale afterSale) {
        OrderItemAfterSaleStatusEnum afterSaleTypeEnum = OrderItemAfterSaleStatusEnum.valueOf(orderItem.getAfterSaleStatus());
        switch (afterSaleTypeEnum) {
            case NOT_APPLIED:
            case NEW:
            case PART_AFTER_SALE:
            case ALREADY_APPLIED:
                if (orderItem.getReturnGoodsNumber().equals(orderItem.getNum())) {
                    orderItem.setAfterSaleStatus(OrderItemAfterSaleStatusEnum.ALREADY_APPLIED.name());
                } else if (orderItem.getReturnGoodsNumber().equals(0)) {
                    orderItem.setAfterSaleStatus(OrderItemAfterSaleStatusEnum.NOT_APPLIED.name());
                } else {
                    orderItem.setAfterSaleStatus(OrderItemAfterSaleStatusEnum.PART_AFTER_SALE.name());
                }
                break;
            default:
                break;
        }

        LambdaUpdateWrapper<OrderItem> lambdaUpdateWrapper = new LambdaUpdateWrapper<OrderItem>()
                .eq(OrderItem::getSn, orderItem.getSn())
                .set(OrderItem::getAfterSaleStatus, orderItem.getAfterSaleStatus())
                .set(OrderItem::getReturnGoodsNumber, orderItem.getReturnGoodsNumber())
                .set(OrderItem::getRefundPrice, orderItem.getRefundPrice());

        if (afterSale.getServiceStatus().equals(AfterSaleStatusEnum.COMPLETE.name())) {
            if (orderItem.getReturnGoodsNumber().equals(orderItem.getNum())) {
                lambdaUpdateWrapper.set(OrderItem::getIsRefund, RefundStatusEnum.ALL_REFUND.name());
            } else {
                lambdaUpdateWrapper.set(OrderItem::getIsRefund, RefundStatusEnum.PART_REFUND.name());
            }
        } else if (afterSale.getServiceStatus().equals(AfterSaleStatusEnum.APPLY.name())) {
            lambdaUpdateWrapper.set(OrderItem::getIsRefund, RefundStatusEnum.REFUNDING.name());
        } else if (afterSale.getServiceStatus().equals(AfterSaleStatusEnum.REFUSE.name()) ||
                afterSale.getServiceStatus().equals(AfterSaleStatusEnum.BUYER_CANCEL.name()) ||
                afterSale.getServiceStatus().equals(AfterSaleStatusEnum.SELLER_TERMINATION.name())) {
            lambdaUpdateWrapper.set(OrderItem::getIsRefund, RefundStatusEnum.NO_REFUND.name());
        }

        orderItemService.update(lambdaUpdateWrapper);
    }
}
