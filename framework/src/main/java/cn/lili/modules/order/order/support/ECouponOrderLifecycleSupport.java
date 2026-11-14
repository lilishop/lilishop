package cn.lili.modules.order.order.support;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.event.TransactionCommitSendMQEvent;
import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.message.LiliMessageTemplate;
import cn.lili.message.MessageSendCallbackBuilder;
import cn.lili.modules.goods.entity.dto.GoodsCompleteMessage;
import cn.lili.modules.order.order.aop.OrderLogPoint;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.order.entity.enums.CommentStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderComplaintStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.entity.enums.RefundStatusEnum;
import cn.lili.modules.order.order.event.OrderCancelledEvent;
import cn.lili.modules.order.order.mapper.OrderItemMapper;
import cn.lili.modules.order.order.mapper.OrderMapper;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.OrderSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.lili.rocketmq.tags.GoodsTagsEnum;
import cn.lili.rocketmq.tags.OrderTagsEnum;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import lombok.RequiredArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * 电子卡券订单完成、取消及状态变更后的收尾能力，不依赖完整订单服务或卡密履约服务。
 * 状态通过条件更新流转；重复完成或取消返回成功，不重复执行收尾。
 * 退款沿用现有礼品卡与渠道实付分摊规则，外部渠道退款不具备数据库事务回滚能力。
 * 日志统一由本组件记录，兼容入口不再重复添加订单操作日志。
 *
 * @author Bulbasaur
 * @since 2026-11-14
 */
@Component
@RequiredArgsConstructor
public class ECouponOrderLifecycleSupport {

    private final OrderMapper orders;
    private final OrderItemMapper items;
    private final SettingService settings;
    private final OrderPaymentSupport payments;
    private final ApplicationEventPublisher events;
    private final RocketmqCustomProperties mqProperties;
    private final LiliMessageTemplate messages;

    /**
     * 将已支付且待交付的电子卡券订单完成；条件更新未生效时返回false，不执行取消或退款。
     *
     * @param orderSn 订单编号
     * @return 已完成或本次完成成功为true，订单不符合条件为false
     */
    @Transactional(rollbackFor = Exception.class)
    @OrderLogPoint(description = "'电子卡券订单['+#orderSn+']系统完成'", orderSn = "#orderSn")
    public boolean complete(String orderSn) {
        Order order = orders.selectOne(new LambdaQueryWrapper<Order>().eq(Order::getSn, orderSn));
        if (order == null || !OrderTypeEnum.E_COUPON.name().equals(order.getOrderType())) {
            return false;
        }
        if (OrderStatusEnum.COMPLETED.name().equals(order.getOrderStatus())) {
            return true;
        }
        if (OrderStatusEnum.CANCELLED.name().equals(order.getOrderStatus())) {
            return false;
        }
        int updated = orders.update(null, new LambdaUpdateWrapper<Order>()
                .eq(Order::getSn, orderSn)
                .eq(Order::getOrderType, OrderTypeEnum.E_COUPON.name())
                .eq(Order::getPayStatus, PayStatusEnum.PAID.name())
                .eq(Order::getOrderStatus, OrderStatusEnum.PAID.name())
                .set(Order::getOrderStatus, OrderStatusEnum.COMPLETED.name()));
        if (updated == 0) {
            return false;
        }
        order.setOrderStatus(OrderStatusEnum.COMPLETED.name());
        completeSideEffects(order);
        return true;
    }

    /**
     * 取消待交付的电子卡券订单；已完成订单不可取消，重复取消不重复退款。
     * refundMoney=false时保留原有赠品规则：仅更新订单及明细，不发送取消事件或状态消息。
     *
     * @param orderSn 订单编号
     * @param reason 取消原因
     * @param refundMoney 是否按现有分摊规则退款
     * @return 已取消或本次取消成功为true，否则为false
     */
    @Transactional(rollbackFor = Exception.class)
    @OrderLogPoint(description = "'电子卡券订单['+#orderSn+']系统取消，原因为：'+#reason", orderSn = "#orderSn")
    public boolean cancel(String orderSn, String reason, Boolean refundMoney) {
        Order order = orders.selectOne(new LambdaQueryWrapper<Order>().eq(Order::getSn, orderSn));
        if (order == null || !OrderTypeEnum.E_COUPON.name().equals(order.getOrderType())) {
            return false;
        }
        if (OrderStatusEnum.CANCELLED.name().equals(order.getOrderStatus())) {
            return true;
        }
        if (OrderStatusEnum.COMPLETED.name().equals(order.getOrderStatus())) {
            return false;
        }
        int updated = orders.update(null, new LambdaUpdateWrapper<Order>()
                .eq(Order::getSn, orderSn)
                .eq(Order::getOrderType, OrderTypeEnum.E_COUPON.name())
                .eq(Order::getOrderStatus, OrderStatusEnum.PAID.name())
                .set(Order::getOrderStatus, OrderStatusEnum.CANCELLED.name())
                .set(Order::getCancelReason, reason));
        if (updated == 0) {
            return false;
        }
        order.setOrderStatus(OrderStatusEnum.CANCELLED.name());
        order.setCancelReason(reason);
        items.update(null, new LambdaUpdateWrapper<OrderItem>().eq(OrderItem::getOrderSn, orderSn)
                .set(OrderItem::getIsRefund, RefundStatusEnum.ALL_REFUND.name()));
        if (Boolean.TRUE.equals(refundMoney)) {
            payments.refundCanceledOrderBySplit(order, reason);
            events.publishEvent(new OrderCancelledEvent(this, order, reason));
            sendStatus(orderSn, OrderStatusEnum.CANCELLED);
        }
        return true;
    }

    /**
     * 更新评价、完成时间和投诉状态，并沿用原有消息时机：状态消息提交后发送，销量消息直接异步发送。
     *
     * @param order 本次完成的订单
     */
    private void completeSideEffects(Order order) {
        String orderSn = order.getSn();
        items.update(null, new LambdaUpdateWrapper<OrderItem>().eq(OrderItem::getOrderSn, orderSn)
                .set(OrderItem::getCommentStatus, CommentStatusEnum.UNFINISHED));
        orders.update(null, new LambdaUpdateWrapper<Order>().eq(Order::getSn, orderSn).set(Order::getCompleteTime, new Date()));
        updateComplaintStatus(orderSn);
        sendStatus(orderSn, OrderStatusEnum.COMPLETED);
        List<GoodsCompleteMessage> completed = new ArrayList<>();
        for (OrderItem item : items.selectList(new LambdaQueryWrapper<OrderItem>().eq(OrderItem::getOrderSn, orderSn))) {
            GoodsCompleteMessage message = new GoodsCompleteMessage();
            message.setGoodsId(item.getGoodsId());
            message.setSkuId(item.getSkuId());
            message.setBuyNum(item.getNum());
            message.setMemberId(order.getMemberId());
            completed.add(message);
        }
        if (!completed.isEmpty()) {
            messages.asyncSend(mqProperties.getGoodsTopic() + ":" + GoodsTagsEnum.BUY_GOODS_COMPLETE.name(),
                    JSONUtil.toJsonStr(completed), MessageSendCallbackBuilder.commonCallback());
        }
    }

    /**
     * 按订单配置处理完成即关闭投诉入口的规则；未配置或仍有投诉期时保持原状态。
     *
     * @param orderSn 订单编号
     */
    private void updateComplaintStatus(String orderSn) {
        Setting setting = settings.get(SettingEnum.ORDER_SETTING.name());
        if (setting == null || CharSequenceUtil.isBlank(setting.getSettingValue())) {
            return;
        }
        OrderSetting config = JSONUtil.toBean(setting.getSettingValue(), OrderSetting.class);
        if (config == null || config.getCloseComplaint() > 0) {
            return;
        }
        items.update(null, new LambdaUpdateWrapper<OrderItem>().eq(OrderItem::getOrderSn, orderSn)
                .set(OrderItem::getComplainStatus, OrderComplaintStatusEnum.EXPIRED.name()));
    }

    /**
     * 发布事务提交后发送的订单状态消息，事务回滚时不广播状态。
     *
     * @param orderSn 订单编号
     * @param status 目标状态
     */
    private void sendStatus(String orderSn, OrderStatusEnum status) {
        OrderMessage message = new OrderMessage();
        message.setOrderSn(orderSn);
        message.setNewStatus(status);
        events.publishEvent(new TransactionCommitSendMQEvent("发送订单状态变更MQ消息",
                mqProperties.getOrderTopic(), OrderTagsEnum.STATUS_CHANGE.name(), JSONUtil.toJsonStr(message)));
    }
}
