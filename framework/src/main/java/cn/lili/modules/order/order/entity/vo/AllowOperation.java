package cn.lili.modules.order.order.entity.vo;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.enums.DeliverStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import io.swagger.annotations.ApiModelProperty;
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

    @ApiModelProperty(value = "可以取消")
    private Boolean cancel = false;

    @ApiModelProperty(value = "可以支付")
    private Boolean pay = false;

    @ApiModelProperty(value = "可以发货")
    private Boolean ship = false;

    @ApiModelProperty(value = "可以收货")
    private Boolean rog = false;

    @ApiModelProperty(value = "是否允许查看物流信息")
    private Boolean showLogistics = false;

    @ApiModelProperty(value = "是否允许更改收货人信息")
    private Boolean editConsignee = false;

    @ApiModelProperty(value = "是否允许更改价格")
    private Boolean editPrice = false;

    @ApiModelProperty(value = "是否可以进行核销")
    private Boolean take = false;


    /**
     * 根据各种状态构建对象
     *
     * @param order
     */
    public AllowOperation(Order order) {

        //获取订单类型
        String status = order.getOrderStatus();
        String payStatus = order.getPayStatus();
        //编辑订单价格 未付款并且是新订单
        if (payStatus.equals(PayStatusEnum.UNPAID.name()) && status.equals(OrderStatusEnum.UNPAID.name())) {
            this.editPrice = true;
        }

        //新订单
        if (CharSequenceUtil.equalsAny(status, OrderStatusEnum.UNPAID.name(), OrderStatusEnum.PAID.name(), OrderStatusEnum.UNDELIVERED.name())) {
            this.cancel = true;
        }
        //新订单，允许支付
        this.pay = status.equals(OrderStatusEnum.UNPAID.name()) && payStatus.equals(PayStatusEnum.UNPAID.name());

        //可编辑订单收件人信息=实物订单 && 订单未发货 && 订单未取消
        this.editConsignee = order.getOrderType().equals(OrderTypeEnum.NORMAL.name())
                && order.getDeliverStatus().equals(DeliverStatusEnum.UNDELIVERED.name())
                && !status.equals(OrderStatusEnum.CANCELLED.name());

        //是否允许被发货
        this.ship = editConsignee && status.equals(OrderStatusEnum.UNDELIVERED.name());

        //是否允许被收货
        this.rog = status.equals(OrderStatusEnum.DELIVERED.name());

        //是否允许查看物流信息
        this.showLogistics = order.getDeliverStatus().equals(DeliverStatusEnum.DELIVERED.name()) && status.equals(OrderStatusEnum.DELIVERED.name());

        this.take = order.getOrderType().equals(OrderTypeEnum.VIRTUAL.name()) && order.getOrderStatus().equals(OrderStatusEnum.TAKE.name());
    }

    /**
     * 根据各种状态构建对象
     *
     * @param order
     */
    public AllowOperation(OrderSimpleVO order) {

        //获取订单类型
        String status = order.getOrderStatus();
        String payStatus = order.getPayStatus();
        //编辑订单价格 未付款并且是新订单
        if (payStatus.equals(PayStatusEnum.UNPAID.name()) && status.equals(OrderStatusEnum.UNPAID.name())) {
            this.editPrice = true;
        }

        //新订单
        if (CharSequenceUtil.equalsAny(status, OrderStatusEnum.UNPAID.name(), OrderStatusEnum.PAID.name(), OrderStatusEnum.UNDELIVERED.name())) {
            this.cancel = true;

        }
        //新订单，允许支付
        this.pay = status.equals(OrderStatusEnum.UNPAID.name());

        //订单未发货，就可以编辑收货人信息
        this.editConsignee = order.getDeliverStatus().equals(DeliverStatusEnum.UNDELIVERED.name());

        //是否允许被发货
        this.ship = editConsignee && status.equals(OrderStatusEnum.UNDELIVERED.name());

        //是否允许被收货
        this.rog = status.equals(OrderStatusEnum.DELIVERED.name());

        //是否允许查看物流信息
        this.showLogistics = order.getDeliverStatus().equals(DeliverStatusEnum.DELIVERED.name()) && status.equals(OrderStatusEnum.DELIVERED.name());

        this.take = order.getOrderType().equals(OrderTypeEnum.VIRTUAL.name()) && order.getOrderStatus().equals(OrderStatusEnum.TAKE.name());
    }


}
