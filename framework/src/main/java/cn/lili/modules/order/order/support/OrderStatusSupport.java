package cn.lili.modules.order.order.support;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.event.TransactionCommitSendMQEvent;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.security.context.UserContext;
import cn.lili.message.LiliMessageTemplate;
import cn.lili.message.MessageSendCallbackBuilder;
import cn.lili.modules.goods.entity.dto.GoodsCompleteMessage;
import cn.lili.modules.member.entity.dto.MemberAddressDTO;
import cn.lili.modules.order.cart.entity.enums.DeliveryMethodEnum;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dos.Receipt;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.order.entity.enums.*;
import cn.lili.modules.order.order.event.OrderCancelledEvent;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.order.mapper.OrderMapper;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import cn.lili.modules.order.order.service.ReceiptService;
import cn.lili.modules.order.trade.entity.dos.OrderLog;
import cn.lili.modules.order.trade.service.OrderLogService;
import cn.lili.modules.promotion.service.PickupCardOrderService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.OrderSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.lili.rocketmq.tags.GoodsTagsEnum;
import cn.lili.rocketmq.tags.OrderTagsEnum;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

/**
 * 订单状态流转与生命周期支撑组件
 * <p>
 * 负责订单买家与系统取消、订单确认、完成收尾与商品销量上报、备注与地址修改以及状态变更消息广播。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Slf4j
@Component
public class OrderStatusSupport {

    private static final String ORDER_SN_COLUMN = "order_sn";

    @Autowired
    private OrderMapper orderMapper;

    @Autowired
    private OrderItemService orderItemService;

    @Autowired
    private OrderLogService orderLogService;

    @Autowired
    private PickupCardOrderService pickupCardOrderService;

    @Autowired
    private SettingService settingService;

    @Autowired
    private LiliMessageTemplate liliMessageTemplate;

    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;

    @Autowired
    private ApplicationEventPublisher applicationEventPublisher;

    @Autowired
    private ReceiptService receiptService;

    @Autowired
    private OrderPaymentSupport orderPaymentSupport;

    public OrderStatusSupport() {
    }

    public OrderStatusSupport(OrderItemService orderItemService,
                              OrderLogService orderLogService,
                              PickupCardOrderService pickupCardOrderService,
                              SettingService settingService,
                              LiliMessageTemplate liliMessageTemplate,
                              RocketmqCustomProperties rocketmqCustomProperties,
                              ApplicationEventPublisher applicationEventPublisher,
                              ReceiptService receiptService,
                              OrderPaymentSupport orderPaymentSupport) {
        this.orderItemService = orderItemService;
        this.orderLogService = orderLogService;
        this.pickupCardOrderService = pickupCardOrderService;
        this.settingService = settingService;
        this.liliMessageTemplate = liliMessageTemplate;
        this.rocketmqCustomProperties = rocketmqCustomProperties;
        this.applicationEventPublisher = applicationEventPublisher;
        this.receiptService = receiptService;
        this.orderPaymentSupport = orderPaymentSupport;
    }

    public void setOrderPaymentSupport(OrderPaymentSupport orderPaymentSupport) {
        this.orderPaymentSupport = orderPaymentSupport;
    }

    private OrderPaymentSupport getOrderPaymentSupport() {
        if (orderPaymentSupport == null) {
            orderPaymentSupport = new OrderPaymentSupport();
        }
        return orderPaymentSupport;
    }

    /**
     * 买家或商户主动取消订单
     * <p>
     * 关键业务规则：
     * <ul>
     *     <li>未成团的拼团订单若处于待成团阶段，禁止主动取消。</li>
     *     <li>仅允许未发货、未支付、自提待提货等可取消状态进行取消。</li>
     *     <li>已支付订单触发礼品卡原路回退与渠道退款分摊，并发布 OrderCancelledEvent 事件驱动流水冲回。</li>
     * </ul>
     *
     * @param orderSn      订单编号
     * @param reason       取消原因
     * @param orderService 订单服务接口
     * @return 取消后的订单实体
     */
    public Order cancel(String orderSn, String reason, OrderService orderService) {
        Order order = OperationalJudgment.judgment(orderService.getBySn(orderSn));
        if (OrderPromotionTypeEnum.PINTUAN.name().equals(order.getOrderPromotionType())
                && !CharSequenceUtil.equalsAny(order.getOrderStatus(), OrderStatusEnum.TAKE.name(),
                OrderStatusEnum.UNDELIVERED.name(), OrderStatusEnum.STAY_PICKED_UP.name())) {
            throw new ServiceException(ResultCode.ORDER_CAN_NOT_CANCEL);
        }
        if (CharSequenceUtil.equalsAny(order.getOrderStatus(),
                OrderStatusEnum.UNDELIVERED.name(),
                OrderStatusEnum.UNPAID.name(),
                OrderStatusEnum.STAY_PICKED_UP.name(),
                OrderStatusEnum.PAID.name(),
                OrderStatusEnum.TAKE.name())) {

            order.setOrderStatus(OrderStatusEnum.CANCELLED.name());
            order.setCancelReason(reason);
            orderService.updateById(order);

            boolean isPaid = PayStatusEnum.PAID.name().equals(order.getPayStatus());
            boolean isPickupOrder = pickupCardOrderService.isPickupOrder(order.getSn());
            boolean needMarkRefund = isPaid && !isPickupOrder;
            orderItemService.update(new LambdaUpdateWrapper<OrderItem>()
                    .eq(OrderItem::getOrderSn, orderSn)
                    .set(needMarkRefund, OrderItem::getIsRefund, RefundStatusEnum.ALL_REFUND.name())
                    .setSql(needMarkRefund, "refund_price = flow_price")
                    .set(OrderItem::getAfterSaleStatus, OrderItemAfterSaleStatusEnum.EXPIRED.name())
                    .set(OrderItem::getComplainStatus, OrderComplaintStatusEnum.EXPIRED.name()));

            getOrderPaymentSupport().refundCanceledOrderBySplit(order, reason);
            if (isPaid || isPickupOrder) {
                applicationEventPublisher.publishEvent(new OrderCancelledEvent(this, order, reason));
            }
            orderStatusMessage(order, orderService);
            return order;
        } else {
            throw new ServiceException(ResultCode.ORDER_CAN_NOT_CANCEL);
        }
    }

    /**
     * 系统自动取消订单（超时关单、拼团失败自动关单等）
     *
     * @param orderSn      订单编号
     * @param reason       取消原因
     * @param refundMoney  是否退款
     */
    public void systemCancel(String orderSn, String reason, Boolean refundMoney) {
        Order order = orderMapper.selectOne(new LambdaQueryWrapper<Order>().eq(Order::getSn, orderSn));
        if (order == null) {
            log.warn("系统自动关单失败，订单[{}]不存在", orderSn);
            return;
        }
        if (OrderStatusEnum.CANCELLED.name().equals(order.getOrderStatus())) {
            log.info("系统自动关单[{}]已处于取消状态，跳过", orderSn);
            return;
        }

        if (Boolean.FALSE.equals(refundMoney)) {
            if (!OrderStatusEnum.UNPAID.name().equals(order.getOrderStatus())) {
                log.warn("系统自动关单拒绝：订单[{}]非待支付状态，拒绝执行不退款关单，当前状态：{}", orderSn, order.getOrderStatus());
                return;
            }
            int updated = orderMapper.update(null, new LambdaUpdateWrapper<Order>()
                    .eq(Order::getSn, orderSn)
                    .eq(Order::getOrderStatus, OrderStatusEnum.UNPAID.name())
                    .set(Order::getOrderStatus, OrderStatusEnum.CANCELLED.name())
                    .set(Order::getCancelReason, reason));
            if (updated == 0) {
                log.warn("系统自动关单冲突：订单[{}]状态已被改变，取消终止", orderSn);
                return;
            }
        } else {
            if (OrderStatusEnum.COMPLETED.name().equals(order.getOrderStatus())) {
                log.warn("系统自动关单拒绝：订单[{}]已完成，禁止系统取消", orderSn);
                return;
            }
            int updated = orderMapper.update(null, new LambdaUpdateWrapper<Order>()
                    .eq(Order::getSn, orderSn)
                    .in(Order::getOrderStatus, Arrays.asList(
                            OrderStatusEnum.UNPAID.name(),
                            OrderStatusEnum.PAID.name(),
                            OrderStatusEnum.UNDELIVERED.name(),
                            OrderStatusEnum.STAY_PICKED_UP.name()
                    ))
                    .set(Order::getOrderStatus, OrderStatusEnum.CANCELLED.name())
                    .set(Order::getCancelReason, reason));
            if (updated == 0) {
                log.warn("系统自动关单冲突：订单[{}]状态已被改变，取消终止", orderSn);
                return;
            }
        }

        order.setOrderStatus(OrderStatusEnum.CANCELLED.name());
        order.setCancelReason(reason);
        boolean isPaid = PayStatusEnum.PAID.name().equals(order.getPayStatus());
        boolean isPickupOrder = pickupCardOrderService.isPickupOrder(order.getSn());
        boolean needMarkRefund = isPaid && !isPickupOrder;
        orderItemService.update(new LambdaUpdateWrapper<OrderItem>()
                .eq(OrderItem::getOrderSn, orderSn)
                .set(needMarkRefund, OrderItem::getIsRefund, RefundStatusEnum.ALL_REFUND.name())
                .setSql(needMarkRefund, "refund_price = flow_price")
                .set(OrderItem::getAfterSaleStatus, OrderItemAfterSaleStatusEnum.EXPIRED.name())
                .set(OrderItem::getComplainStatus, OrderComplaintStatusEnum.EXPIRED.name()));

        if (Boolean.TRUE.equals(refundMoney) || !PayStatusEnum.PAID.name().equals(order.getPayStatus()) || pickupCardOrderService.isPickupOrder(order.getSn())) {
            getOrderPaymentSupport().refundCanceledOrderBySplit(order, reason);
        }
        if (PayStatusEnum.PAID.name().equals(order.getPayStatus()) || pickupCardOrderService.isPickupOrder(order.getSn())) {
            applicationEventPublisher.publishEvent(new OrderCancelledEvent(this, order, reason));
        }
        OrderMessage message = new OrderMessage();
        message.setOrderSn(orderSn);
        message.setNewStatus(OrderStatusEnum.CANCELLED);
        sendUpdateStatusMessage(message);
    }

    /**
     * 普通实物订单确认
     *
     * @param orderSn      订单编号
     * @param orderService 订单服务接口
     */
    public void normalOrderConfirm(String orderSn, OrderService orderService) {
        OrderStatusEnum orderStatusEnum = null;
        Order order = orderService.getBySn(orderSn);
        if (DeliveryMethodEnum.SELF_PICK_UP.name().equals(order.getDeliveryMethod())) {
            orderStatusEnum = OrderStatusEnum.STAY_PICKED_UP;
        } else if (DeliveryMethodEnum.LOGISTICS.name().equals(order.getDeliveryMethod())) {
            orderStatusEnum = OrderStatusEnum.UNDELIVERED;
        }
        orderService.update(new LambdaUpdateWrapper<Order>()
                .eq(Order::getSn, orderSn)
                .set(Order::getOrderStatus, orderStatusEnum.name()));
        OrderMessage orderMessage = new OrderMessage();
        orderMessage.setNewStatus(orderStatusEnum);
        orderMessage.setOrderSn(orderSn);
        orderService.sendUpdateStatusMessage(orderMessage);
    }

    /**
     * 虚拟商品订单确认
     *
     * @param orderSn      订单编号
     * @param orderService 订单服务接口
     */
    public void virtualOrderConfirm(String orderSn, OrderService orderService) {
        Order order = orderService.getBySn(orderSn);
        if (order == null || !OrderTypeEnum.VIRTUAL.name().equals(order.getOrderType())) {
            return;
        }
        boolean updated = orderService.update(new LambdaUpdateWrapper<Order>()
                .eq(Order::getSn, orderSn)
                .eq(Order::getOrderType, OrderTypeEnum.VIRTUAL.name())
                .eq(Order::getOrderStatus, OrderStatusEnum.PAID.name())
                .set(Order::getOrderStatus, OrderStatusEnum.TAKE.name()));
        if (!updated) {
            return;
        }
        OrderMessage orderMessage = new OrderMessage();
        orderMessage.setNewStatus(OrderStatusEnum.TAKE);
        orderMessage.setOrderSn(orderSn);
        orderService.sendUpdateStatusMessage(orderMessage);
    }

    /**
     * 订单完成收尾与销量上报
     *
     * @param order        订单实体
     * @param orderSn      订单编号
     */
    public void complete(Order order, String orderSn) {
        orderMapper.update(null, new LambdaUpdateWrapper<Order>().eq(Order::getSn, orderSn).set(Order::getOrderStatus, OrderStatusEnum.COMPLETED.name()));
        orderItemService.update(new UpdateWrapper<OrderItem>().eq(ORDER_SN_COLUMN, orderSn)
                .set("comment_status", CommentStatusEnum.UNFINISHED));
        orderMapper.update(null, new LambdaUpdateWrapper<Order>().eq(Order::getSn, orderSn).set(Order::getCompleteTime, new Date()));

        updateOrderComplainStatus(orderSn);

        OrderMessage orderMessage = new OrderMessage();
        orderMessage.setNewStatus(OrderStatusEnum.COMPLETED);
        orderMessage.setOrderSn(order.getSn());
        sendUpdateStatusMessage(orderMessage);

        List<OrderItem> orderItems = orderItemService.getByOrderSn(orderSn);
        List<GoodsCompleteMessage> goodsCompleteMessageList = new ArrayList<>();
        for (OrderItem orderItem : orderItems) {
            GoodsCompleteMessage goodsCompleteMessage = new GoodsCompleteMessage();
            goodsCompleteMessage.setGoodsId(orderItem.getGoodsId());
            goodsCompleteMessage.setSkuId(orderItem.getSkuId());
            goodsCompleteMessage.setBuyNum(orderItem.getNum());
            goodsCompleteMessage.setMemberId(order.getMemberId());
            goodsCompleteMessageList.add(goodsCompleteMessage);
        }
        if (!goodsCompleteMessageList.isEmpty()) {
            String destination = rocketmqCustomProperties.getGoodsTopic() + ":" + GoodsTagsEnum.BUY_GOODS_COMPLETE.name();
            liliMessageTemplate.asyncSend(destination, JSONUtil.toJsonStr(goodsCompleteMessageList), MessageSendCallbackBuilder.commonCallback());
        }
    }

    /**
     * 发送订单状态变更 MQ 广播消息
     *
     * @param order        订单实体
     * @param orderService 订单服务接口
     */
    public void orderStatusMessage(Order order, OrderService orderService) {
        OrderMessage orderMessage = new OrderMessage();
        orderMessage.setOrderSn(order.getSn());
        orderMessage.setNewStatus(OrderStatusEnum.valueOf(order.getOrderStatus()));
        orderService.sendUpdateStatusMessage(orderMessage);
    }

    /**
     * 发送事务提交后订单状态变更 MQ 消息
     *
     * @param orderMessage 订单变更消息
     */
    public void sendUpdateStatusMessage(OrderMessage orderMessage) {
        applicationEventPublisher.publishEvent(new TransactionCommitSendMQEvent("发送订单状态变更MQ消息",
                rocketmqCustomProperties.getOrderTopic(),
                OrderTagsEnum.STATUS_CHANGE.name(), JSONUtil.toJsonStr(orderMessage)));
    }

    /**
     * 逻辑删除订单及其关联订单项
     *
     * @param sn           订单编号
     * @param orderService 订单服务接口
     */
    public void deleteOrder(String sn, OrderService orderService) {
        Order order = orderService.getBySn(sn);
        if (order == null) {
            log.error("编号为" + sn + "的订单不存在！");
            throw new ServiceException();
        }
        orderService.update(new LambdaUpdateWrapper<Order>().eq(Order::getSn, sn).set(Order::getDeleteFlag, true));
        orderItemService.update(new LambdaUpdateWrapper<OrderItem>().eq(OrderItem::getOrderSn, sn).set(OrderItem::getDeleteFlag, true));
    }

    /**
     * 开具发票
     *
     * @param sn 订单编号
     * @return 是否成功
     */
    public Boolean invoice(String sn) {
        Receipt receipt = receiptService.getByOrderSn(sn);
        if (receipt != null) {
            receipt.setReceiptStatus(1);
            return receiptService.updateById(receipt);
        }
        throw new ServiceException(ResultCode.USER_RECEIPT_NOT_EXIST);
    }

    /**
     * 更新商家备注
     *
     * @param orderSn      订单编号
     * @param sellerRemark 商家备注
     * @param orderService 订单服务接口
     * @return 更新后的订单实体
     */
    public Order updateSellerRemark(String orderSn, String sellerRemark, OrderService orderService) {
        Order order = OperationalJudgment.judgment(orderService.getBySn(orderSn));
        order.setSellerRemark(sellerRemark);
        orderService.updateById(order);
        return order;
    }

    /**
     * 修改订单收货人信息
     *
     * @param orderSn          订单编号
     * @param memberAddressDTO 收货地址传输对象
     * @param orderService     订单服务接口
     * @return 更新后的订单实体
     */
    public Order updateConsignee(String orderSn, MemberAddressDTO memberAddressDTO, OrderService orderService) {
        Order order = OperationalJudgment.judgment(orderService.getBySn(orderSn));
        String message = "订单[" + orderSn + "]收货地址修改，由[" + order.getConsigneeDetail() + "]修改为[" + memberAddressDTO.getConsigneeDetail() + "]";
        BeanUtil.copyProperties(memberAddressDTO, order);
        orderService.updateById(order);

        OrderLog orderLog = new OrderLog(orderSn, UserContext.getCurrentUser().getId(),
                UserContext.getCurrentUser().getRole().getRole(),
                UserContext.getCurrentUser().getUsername(), message);
        orderLogService.save(orderLog);
        return order;
    }

    /**
     * 根据系统配置判定是否自动关闭投诉状态
     *
     * @param orderSn 订单编号
     */
    public void updateOrderComplainStatus(String orderSn) {
        Setting setting = settingService.get(SettingEnum.ORDER_SETTING.name());
        if (setting == null || CharSequenceUtil.isBlank(setting.getSettingValue())) {
            return;
        }
        OrderSetting orderSetting = JSONUtil.toBean(setting.getSettingValue(), OrderSetting.class);
        if (orderSetting == null || orderSetting.getCloseComplaint() > 0) {
            return;
        }
        LambdaUpdateWrapper<OrderItem> lambdaUpdateWrapper = new LambdaUpdateWrapper<OrderItem>()
                .eq(OrderItem::getOrderSn, orderSn)
                .set(OrderItem::getComplainStatus, OrderComplaintStatusEnum.EXPIRED.name());
        orderItemService.update(lambdaUpdateWrapper);
    }

    /**
     * 更新订单状态
     *
     * @param orderSn      订单编号
     * @param orderStatus  订单状态枚举
     * @param orderService 订单服务接口
     */
    public void updateStatus(String orderSn, OrderStatusEnum orderStatus, OrderService orderService) {
        orderService.update(new LambdaUpdateWrapper<Order>()
                .eq(Order::getSn, orderSn)
                .set(Order::getOrderStatus, orderStatus.name()));
    }
}
