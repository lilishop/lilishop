package cn.lili.modules.order.order.entity.vo;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.modules.order.cart.entity.enums.DeliveryMethodEnum;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.enums.DeliverStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 订单可进行的操作
 *
 * @author Chopper
 * @since 2020/11/17 7:29 下午
 */
@Data
public class AllowOperation implements Serializable {

    private static final long serialVersionUID = -5109440403955543227L;

    @Schema(description = "可以取消")
    private Boolean cancel = false;

    @Schema(description = "可以支付")
    private Boolean pay = false;

    @Schema(description = "可以发货")
    private Boolean ship = false;

    @Schema(description = "可以收货")
    private Boolean rog = false;

    @Schema(description = "是否允许查看物流信息")
    private Boolean showLogistics = false;

    @Schema(description = "是否允许更改收货人信息")
    private Boolean editConsignee = false;

    @Schema(description = "是否允许更改价格")
    private Boolean editPrice = false;

    @Schema(description = "是否可以进行核销")
    private Boolean take = false;

    @Schema(description = "是否允许整单发货")
    private Boolean fulfillWhole = false;

    @Schema(description = "是否允许拆单发货")
    private Boolean fulfillSplit = false;

    @Schema(description = "是否允许无需配送发货")
    private Boolean fulfillNoDelivery = false;

    /**
     * 根据各种状态构建对象
     *
     * @param order 订单
     */
    public AllowOperation(Order order) {

        // APPOINTMENT：未支付可取消/支付；已支付不可取消（走售后）；无物流/核销
        if (OrderTypeEnum.APPOINTMENT.name().equals(order.getOrderType())) {
            String status = order.getOrderStatus();
            String payStatus = order.getPayStatus();
            boolean unpaid = PayStatusEnum.UNPAID.name().equals(payStatus)
                    && OrderStatusEnum.UNPAID.name().equals(status);
            this.cancel = unpaid;
            this.pay = unpaid;
            this.ship = false;
            this.rog = false;
            this.showLogistics = false;
            this.editConsignee = false;
            this.editPrice = false;
            this.take = false;
            return;
        }

        // E_COUPON：仅 UNPAID 可取消/支付；无物流/核销/改价；无售后操作项
        if (OrderTypeEnum.E_COUPON.name().equals(order.getOrderType())) {
            String status = order.getOrderStatus();
            String payStatus = order.getPayStatus();
            this.cancel = OrderStatusEnum.UNPAID.name().equals(status) && PayStatusEnum.UNPAID.name().equals(payStatus);
            this.pay = this.cancel;
            this.ship = false;
            this.rog = false;
            this.showLogistics = false;
            this.editConsignee = false;
            this.editPrice = false;
            this.take = false;
            return;
        }

        //获取订单类型
        String status = order.getOrderStatus();
        String payStatus = order.getPayStatus();
        //编辑订单价格 未付款并且是新订单
        if (payStatus.equals(PayStatusEnum.UNPAID.name()) && status.equals(OrderStatusEnum.UNPAID.name())) {
            this.editPrice = true;
        }

        //新订单
        if (CharSequenceUtil.equalsAny(status, OrderStatusEnum.UNPAID.name(), OrderStatusEnum.PAID.name(), OrderStatusEnum.UNDELIVERED.name(),
                OrderStatusEnum.STAY_PICKED_UP.name(), OrderStatusEnum.TAKE.name())) {
            this.cancel = true;
        }
        //新订单，允许支付
        this.pay = status.equals(OrderStatusEnum.UNPAID.name()) && payStatus.equals(PayStatusEnum.UNPAID.name());

        //可编辑订单收件人信息=实物订单 && 订单未发货 && 订单未取消 && 订单不是自提
        this.editConsignee = order.getOrderType().equals(OrderTypeEnum.NORMAL.name())
                && (order.getDeliverStatus().equals(DeliverStatusEnum.UNDELIVERED.name()) || order.getDeliverStatus().equals(DeliverStatusEnum.PARTS_DELIVERED.name()))
                && !status.equals(OrderStatusEnum.CANCELLED.name())
                && !order.getDeliveryMethod().equals(DeliveryMethodEnum.SELF_PICK_UP.name());

        //是否允许被发货
        this.ship = editConsignee && (status.equals(OrderStatusEnum.UNDELIVERED.name()) || order.getDeliverStatus().equals(DeliverStatusEnum.PARTS_DELIVERED.name()));

        applyFulfillmentModes(order, status);

        //是否允许被收货
        this.rog = status.equals(OrderStatusEnum.DELIVERED.name());

        //是否允许查看物流信息（含无需配送配送说明页）
        this.showLogistics = (order.getDeliverStatus().equals(DeliverStatusEnum.DELIVERED.name()) || order.getDeliverStatus().equals(DeliverStatusEnum.PARTS_DELIVERED.name()))
                && (status.equals(OrderStatusEnum.DELIVERED.name()) || status.equals(OrderStatusEnum.PARTS_DELIVERED.name()));

        //虚拟订单 或 自提订单可以核销
        this.take =
                (order.getOrderType().equals(OrderTypeEnum.VIRTUAL.name()) && order.getOrderStatus().equals(OrderStatusEnum.TAKE.name()))
                        || (order.getDeliveryMethod().equals(DeliveryMethodEnum.SELF_PICK_UP.name()) && order.getOrderStatus().equals(OrderStatusEnum.STAY_PICKED_UP.name()));
    }

    /**
     * 根据各种状态构建对象
     *
     * @param order 列表订单
     */
    public AllowOperation(OrderSimpleVO order) {

        // APPOINTMENT 列表页操作权限，规则同 {@link #AllowOperation(Order)}
        if (OrderTypeEnum.APPOINTMENT.name().equals(order.getOrderType())) {
            String status = order.getOrderStatus();
            String payStatus = order.getPayStatus();
            boolean unpaid = PayStatusEnum.UNPAID.name().equals(payStatus)
                    && OrderStatusEnum.UNPAID.name().equals(status);
            this.cancel = unpaid;
            this.pay = unpaid;
            this.ship = false;
            this.rog = false;
            this.showLogistics = false;
            this.editConsignee = false;
            this.editPrice = false;
            this.take = false;
            return;
        }

        // E_COUPON 列表页操作权限，规则同 {@link #AllowOperation(Order)}
        if (OrderTypeEnum.E_COUPON.name().equals(order.getOrderType())) {
            String status = order.getOrderStatus();
            String payStatus = order.getPayStatus();
            this.cancel = OrderStatusEnum.UNPAID.name().equals(status) && PayStatusEnum.UNPAID.name().equals(payStatus);
            this.pay = this.cancel;
            this.ship = false;
            this.rog = false;
            this.showLogistics = false;
            this.editConsignee = false;
            this.editPrice = false;
            this.take = false;
            return;
        }

        //获取订单类型
        String status = order.getOrderStatus();
        String payStatus = order.getPayStatus();
        //编辑订单价格 未付款并且是新订单
        if (payStatus.equals(PayStatusEnum.UNPAID.name()) && status.equals(OrderStatusEnum.UNPAID.name())) {
            this.editPrice = true;
        }

        //取消判定
        if (CharSequenceUtil.equalsAny(status, OrderStatusEnum.UNPAID.name(), OrderStatusEnum.PAID.name(), OrderStatusEnum.UNDELIVERED.name(),
                OrderStatusEnum.PARTS_DELIVERED.name())) {
            this.cancel = true;
        }
        //新订单，允许支付
        this.pay = status.equals(OrderStatusEnum.UNPAID.name());

        //订单未发货或部分发货，就可以编辑收货人信息
        this.editConsignee = order.getOrderType().equals(OrderTypeEnum.NORMAL.name())
                && (order.getDeliverStatus().equals(DeliverStatusEnum.UNDELIVERED.name()) || order.getDeliverStatus().equals(DeliverStatusEnum.PARTS_DELIVERED.name()));

        //是否允许被发货
        this.ship = editConsignee && (status.equals(OrderStatusEnum.UNDELIVERED.name()) || status.equals(OrderStatusEnum.PARTS_DELIVERED.name()));

        applyFulfillmentModesForList(order, status);

        //是否允许被收货
        this.rog = status.equals(OrderStatusEnum.DELIVERED.name());

        //是否允许查看物流信息
        this.showLogistics = (order.getDeliverStatus().equals(DeliverStatusEnum.DELIVERED.name()) || order.getDeliverStatus().equals(DeliverStatusEnum.PARTS_DELIVERED.name()))
                && (status.equals(OrderStatusEnum.DELIVERED.name()) || status.equals(OrderStatusEnum.PARTS_DELIVERED.name()));

        this.take = order.getOrderType().equals(OrderTypeEnum.VIRTUAL.name()) && order.getOrderStatus().equals(OrderStatusEnum.TAKE.name());
    }

    private void applyFulfillmentModes(Order order, String status) {
        if (!Boolean.TRUE.equals(this.ship)) {
            return;
        }
        if (OrderStatusEnum.UNDELIVERED.name().equals(status)) {
            this.fulfillWhole = true;
            this.fulfillSplit = true;
            this.fulfillNoDelivery = true;
        } else if (OrderStatusEnum.PARTS_DELIVERED.name().equals(status)
                || DeliverStatusEnum.PARTS_DELIVERED.name().equals(order.getDeliverStatus())) {
            this.fulfillSplit = true;
        }
    }

    private void applyFulfillmentModesForList(OrderSimpleVO order, String status) {
        if (!Boolean.TRUE.equals(this.ship)) {
            return;
        }
        if (OrderStatusEnum.UNDELIVERED.name().equals(status)) {
            this.fulfillWhole = true;
            this.fulfillSplit = true;
            this.fulfillNoDelivery = true;
        } else if (OrderStatusEnum.PARTS_DELIVERED.name().equals(status)
                || DeliverStatusEnum.PARTS_DELIVERED.name().equals(order.getDeliverStatus())) {
            this.fulfillSplit = true;
        }
    }
}
