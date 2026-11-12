package cn.lili.modules.order.order.support;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.utils.SnowFlake;
import cn.lili.common.utils.SpringContextUtil;
import cn.lili.modules.distribution.event.DistributionPaymentRewardsEvent;
import cn.lili.modules.distribution.event.DistributionRecruitAutoApplyEvent;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.event.OrderPaidEvent;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.trade.entity.dos.OrderLog;
import cn.lili.modules.order.trade.service.OrderLogService;
import cn.lili.modules.payment.entity.RefundLog;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.modules.payment.kit.Payment;
import cn.lili.modules.promotion.service.GiftCardCashService;
import cn.lili.modules.promotion.service.PickupCardOrderService;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Date;

/**
 * 订单支付与退款结算支撑组件
 * <p>
 * 关键业务规则与边界约束：
 * <ul>
 *     <li>支付记账采用 CAS 严格原子流转（要求前置状态为待付款 UNPAID），防止网络重试与多渠道回调并发竞态。</li>
 *     <li>对已取消订单迟到支付的异常场景，严格执行拒绝复活策略，触发自动原路退款；对并发记账成功后的第二笔流水，判定为重复付款自动原路退回。</li>
 *     <li>取消关单退款严格遵循分摊结算：优先核销回退礼品卡本金余额，剩余渠道实付金额再调起第三方支付原路退款。</li>
 *     <li>支付成功后通过 Spring 领域事件解耦发布流水记录（OrderPaidEvent）与分销奖励判定，不强依赖下游业务模块。</li>
 * </ul>
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Slf4j
@Component
public class OrderPaymentSupport {

    @Autowired
    private PickupCardOrderService pickupCardOrderService;

    @Autowired
    private GiftCardCashService giftCardCashService;

    @Autowired
    private OrderLogService orderLogService;

    @Autowired
    private ApplicationEventPublisher applicationEventPublisher;

    public OrderPaymentSupport() {
    }

    public OrderPaymentSupport(PickupCardOrderService pickupCardOrderService,
                               GiftCardCashService giftCardCashService,
                               OrderLogService orderLogService,
                               ApplicationEventPublisher applicationEventPublisher) {
        this.pickupCardOrderService = pickupCardOrderService;
        this.giftCardCashService = giftCardCashService;
        this.orderLogService = orderLogService;
        this.applicationEventPublisher = applicationEventPublisher;
    }

    /**
     * 将双精度浮点数转为标准金额（保留2位小数，四舍五入）
     *
     * @param value 浮点金额
     * @return BigDecimal金额
     */
    public BigDecimal toMoney(Double value) {
        return value == null ? BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP)
                : BigDecimal.valueOf(value).setScale(2, RoundingMode.HALF_UP);
    }

    /**
     * 订单支付核心逻辑
     * <p>
     * 关键业务规则与并发控制：
     * <ul>
     *     <li>使用 CAS 原子更新（状态为 UNPAID 时流转为 PAID），防重复支付与并发竞态。</li>
     *     <li>针对已取消订单的延迟到账，拒绝复活订单，直接执行原路退款。</li>
     *     <li>支付成功发布 OrderPaidEvent 领域事件与事务提交 MQ 事件，解耦触发流水与分销。</li>
     * </ul>
     *
     * @param orderSn       订单编号
     * @param paymentMethod 支付方式
     * @param receivableNo  第三方支付流水号
     * @param price         校验金额
     * @param orderService  订单服务接口
     */
    public void payOrder(String orderSn, String paymentMethod, String receivableNo, Double price, OrderService orderService) {
        Order order = orderService.getBySn(orderSn);
        if (order == null) {
            log.error("订单[ {} ]不存在，无法支付", orderSn);
            throw new ServiceException(ResultCode.ORDER_NOT_EXIST);
        }

        if (price != null) {
            BigDecimal paidAmount = toMoney(price);
            BigDecimal orderAmount = toMoney(order.getFlowPrice());
            if (paidAmount.compareTo(orderAmount) != 0) {
                log.error("订单[{}]支付金额不一致，拒绝记账，通知金额={}, 订单应付={}",
                        orderSn, paidAmount, orderAmount);
                throw new ServiceException(ResultCode.PAY_INCONSISTENT_ERROR);
            }
        }

        if (PayStatusEnum.PAID.name().equals(order.getPayStatus())) {
            log.error("订单[ {} ]检测到重复付款，请检查处理", orderSn);
            throw new ServiceException(ResultCode.PAY_DOUBLE_ERROR);
        }

        if (OrderStatusEnum.CANCELLED.name().equals(order.getOrderStatus())) {
            log.warn("订单[{}]已处于取消状态，收到延迟支付通知，拒绝复活订单，执行原路自动退款：paymentMethod={}, receivableNo={}",
                    orderSn, paymentMethod, receivableNo);
            refundLatePaymentForCancelledOrder(order, paymentMethod, receivableNo);
            return;
        }

        if (!OrderStatusEnum.UNPAID.name().equals(order.getOrderStatus())) {
            log.warn("订单[{}]非待付款状态，拒绝执行支付记账，orderStatus={}, payStatus={}",
                    orderSn, order.getOrderStatus(), order.getPayStatus());
            throw new ServiceException(ResultCode.PAY_BAN);
        }

        boolean pickupOrder = pickupCardOrderService.isPickupOrder(orderSn);

        boolean updated = orderService.update(new LambdaUpdateWrapper<Order>()
                .eq(Order::getSn, orderSn)
                .eq(Order::getOrderStatus, OrderStatusEnum.UNPAID.name())
                .eq(Order::getPayStatus, PayStatusEnum.UNPAID.name())
                .set(Order::getPaymentTime, new Date())
                .set(Order::getPaymentMethod, paymentMethod)
                .set(Order::getPayStatus, PayStatusEnum.PAID.name())
                .set(Order::getOrderStatus, OrderStatusEnum.PAID.name())
                .set(Order::getReceivableNo, receivableNo)
                .set(Order::getCanReturn, !pickupOrder && !PaymentMethodEnum.BANK_TRANSFER.name().equals(paymentMethod)));

        if (!updated) {
            Order fresh = orderService.getBySn(orderSn);
            if (fresh != null && PayStatusEnum.PAID.name().equals(fresh.getPayStatus())) {
                if (CharSequenceUtil.isNotEmpty(receivableNo) && !receivableNo.equals(fresh.getReceivableNo())) {
                    log.warn("订单[{}]并发期间已被支付流水[{}]记账，当前流水[{}]判定为重复付款，执行原路自动退款",
                            orderSn, fresh.getReceivableNo(), receivableNo);
                    refundDuplicatePayment(fresh, paymentMethod, receivableNo);
                    return;
                }
                log.warn("订单[{}]已被其他线程成功记账，跳过重复处理", orderSn);
                return;
            }
            if (fresh != null && OrderStatusEnum.CANCELLED.name().equals(fresh.getOrderStatus())) {
                log.warn("订单[{}]并发期间已被取消，拒绝复活订单，执行原路自动退款：paymentMethod={}, receivableNo={}",
                        orderSn, paymentMethod, receivableNo);
                refundLatePaymentForCancelledOrder(fresh, paymentMethod, receivableNo);
                return;
            }
            log.error("订单[{}]支付状态CAS更新失败，当前状态异常", orderSn);
            throw new ServiceException(ResultCode.PAY_ERROR);
        }

        order.setPaymentTime(new Date());
        order.setPaymentMethod(paymentMethod);
        order.setPayStatus(PayStatusEnum.PAID.name());
        order.setOrderStatus(OrderStatusEnum.PAID.name());
        order.setReceivableNo(receivableNo);
        order.setCanReturn(!pickupOrder && !PaymentMethodEnum.BANK_TRANSFER.name().equals(paymentMethod));

        // 发布订单支付领域事件（解耦触发店铺财务流水记录）
        applicationEventPublisher.publishEvent(new OrderPaidEvent(this, order));

        OrderMessage orderMessage = new OrderMessage();
        orderMessage.setOrderSn(order.getSn());
        orderMessage.setPaymentMethod(paymentMethod);
        orderMessage.setNewStatus(OrderStatusEnum.PAID);
        orderService.sendUpdateStatusMessage(orderMessage);

        if (!pickupOrder) {
            applicationEventPublisher.publishEvent(new DistributionPaymentRewardsEvent(this, orderSn));
            if (CharSequenceUtil.isNotBlank(order.getMemberId())) {
                applicationEventPublisher.publishEvent(new DistributionRecruitAutoApplyEvent(this, order.getMemberId()));
            }
        }
        String message = "订单付款，付款方式[" + PaymentMethodEnum.valueOf(paymentMethod).paymentName() + "]";
        OrderLog orderLog = new OrderLog(orderSn, "-1", UserEnums.SYSTEM.getRole(), "系统操作", message);
        orderLogService.save(orderLog);
    }

    /**
     * 已取消订单收到延迟支付通知时的退款处理
     *
     * @param order         已取消订单实体
     * @param paymentMethod 支付方式
     * @param receivableNo  第三方流水号
     */
    public void refundLatePaymentForCancelledOrder(Order order, String paymentMethod, String receivableNo) {
        if (order == null || pickupCardOrderService.isPickupOrder(order.getSn())) {
            return;
        }
        BigDecimal refundAmount = toMoney(order.getFlowPrice());
        if (refundAmount.compareTo(BigDecimal.ZERO) <= 0) {
            log.warn("已取消订单[{}]实付金额为0，无需发起退款", order.getSn());
            return;
        }
        PaymentMethodEnum paymentMethodEnum = PaymentMethodEnum.paymentNameOf(paymentMethod);
        if (paymentMethodEnum == null) {
            log.error("已取消订单[{}]退款失败：未识别的支付方式[{}]", order.getSn(), paymentMethod);
            return;
        }
        RefundLog refundLog = RefundLog.builder()
                .isRefund(false)
                .totalAmount(refundAmount.doubleValue())
                .payPrice(refundAmount.doubleValue())
                .memberId(order.getMemberId())
                .paymentName(paymentMethod)
                .afterSaleNo("LATE_PAY_REFUND")
                .paymentReceivableNo(receivableNo)
                .outOrderNo("RF" + SnowFlake.getIdStr())
                .orderSn(order.getSn())
                .refundReason("已取消订单迟到支付自动原路退款")
                .build();
        try {
            Payment payment = (Payment) SpringContextUtil.getBean(paymentMethodEnum.getPlugin());
            if (payment != null) {
                payment.refund(refundLog);
                log.info("已取消订单[{}]迟到支付原路退款成功，receivableNo={}, refundAmount={}",
                        order.getSn(), receivableNo, refundAmount);
            }
        } catch (Exception e) {
            log.error("已取消订单[{}]迟到支付原路退款失败，receivableNo={}", order.getSn(), receivableNo, e);
        }
        String logMsg = "已取消订单收到延迟支付通知[方式: " + paymentMethod + "，流水: " + receivableNo + "]，自动原路退款";
        OrderLog orderLog = new OrderLog(order.getSn(), "-1", UserEnums.SYSTEM.getRole(), "系统操作", logMsg);
        orderLogService.save(orderLog);
    }

    /**
     * 重复支付退款
     *
     * @param order         订单实体
     * @param paymentMethod 支付方式
     * @param receivableNo  第三方流水号
     */
    public void refundDuplicatePayment(Order order, String paymentMethod, String receivableNo) {
        if (order == null || pickupCardOrderService.isPickupOrder(order.getSn())) {
            return;
        }
        BigDecimal refundAmount = toMoney(order.getFlowPrice());
        if (refundAmount.compareTo(BigDecimal.ZERO) <= 0) {
            log.warn("已支付订单[{}]实付金额为0，无需发起退款", order.getSn());
            return;
        }
        PaymentMethodEnum paymentMethodEnum = PaymentMethodEnum.paymentNameOf(paymentMethod);
        if (paymentMethodEnum == null) {
            log.error("已支付订单[{}]重复支付退款失败：未识别的支付方式[{}]", order.getSn(), paymentMethod);
            return;
        }
        RefundLog refundLog = RefundLog.builder()
                .isRefund(false)
                .totalAmount(refundAmount.doubleValue())
                .payPrice(refundAmount.doubleValue())
                .memberId(order.getMemberId())
                .paymentName(paymentMethod)
                .afterSaleNo("DUP_PAY_" + SnowFlake.getIdStr())
                .paymentReceivableNo(receivableNo)
                .outOrderNo("RF" + SnowFlake.getIdStr())
                .orderSn(order.getSn())
                .refundReason("重复支付第二笔款项自动原路退回")
                .build();
        try {
            Payment payment = (Payment) SpringContextUtil.getBean(paymentMethodEnum.getPlugin());
            if (payment != null) {
                payment.refund(refundLog);
                log.info("已支付订单[{}]重复支付原路退款成功，receivableNo={}, refundAmount={}",
                        order.getSn(), receivableNo, refundAmount);
            }
        } catch (Exception e) {
            log.error("已支付订单[{}]重复支付原路退款失败，receivableNo={}", order.getSn(), receivableNo, e);
        }
        String logMsg = "已支付订单收到重复通知[方式: " + paymentMethod + "，流水: " + receivableNo + "]，自动原路退款";
        OrderLog orderLog = new OrderLog(order.getSn(), "-1", UserEnums.SYSTEM.getRole(), "系统操作", logMsg);
        orderLogService.save(orderLog);
    }

    /**
     * 取消订单退款统一分摊：礼品卡回退 + 渠道退款
     *
     * @param order  订单实体
     * @param reason 退款原因
     */
    public void refundCanceledOrderBySplit(Order order, String reason) {
        if (order == null || pickupCardOrderService.isPickupOrder(order.getSn())) {
            return;
        }
        if (!PayStatusEnum.PAID.name().equals(order.getPayStatus())) {
            giftCardCashService.resolveChannelRefundAfterGiftCard(order.getSn(), BigDecimal.ZERO);
            return;
        }
        BigDecimal channelRefund = giftCardCashService.resolveChannelRefundAfterGiftCard(order.getSn(), toMoney(order.getFlowPrice()));
        if (channelRefund.compareTo(BigDecimal.ZERO) <= 0) {
            return;
        }
        if (CharSequenceUtil.isBlank(order.getPaymentMethod())) {
            throw new ServiceException(ResultCode.PAY_NOT_SUPPORT);
        }
        RefundLog refundLog = RefundLog.builder()
                .isRefund(false)
                .totalAmount(channelRefund.doubleValue())
                .payPrice(channelRefund.doubleValue())
                .memberId(order.getMemberId())
                .paymentName(order.getPaymentMethod())
                .afterSaleNo("ORDER_CANCEL")
                .paymentReceivableNo(order.getReceivableNo())
                .outOrderNo("OC" + SnowFlake.getIdStr())
                .orderSn(order.getSn())
                .refundReason(reason)
                .build();
        PaymentMethodEnum paymentMethodEnum = PaymentMethodEnum.paymentNameOf(order.getPaymentMethod());
        if (paymentMethodEnum == null) {
            throw new ServiceException(ResultCode.PAY_NOT_SUPPORT);
        }
        Payment payment = (Payment) SpringContextUtil.getBean(paymentMethodEnum.getPlugin());
        payment.refund(refundLog);
    }
}
