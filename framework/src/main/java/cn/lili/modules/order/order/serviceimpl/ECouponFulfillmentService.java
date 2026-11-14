package cn.lili.modules.order.order.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.goods.service.GiftFulfillmentAlertPublisher;
import cn.lili.modules.goods.support.CardKeyAllocationSupport;
import cn.lili.modules.goods.support.CardKeyTradeSupport;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.mapper.OrderMapper;
import cn.lili.modules.order.order.support.ECouponOrderLifecycleSupport;
import cn.lili.modules.order.trade.entity.dos.OrderLog;
import cn.lili.modules.order.trade.service.OrderLogService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * 电子卡券的支付后履约编排，协调普通购买、满赠及拼团的卡密操作和订单生命周期。
 * 使用订单行锁及已支付状态门禁处理支付后重复消息；拼团先预占，成团后确认交付。
 * 满赠失败仅取消赠品子单并告警，不取消或退款主单。
 * 分配失败后的作废、库存同步与取消参与原事务，不引入独立补偿事务或自动退款重试。
 *
 * @author Bulbasaur
 * @since 2026-11-14
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class ECouponFulfillmentService {

    private final OrderMapper orders;
    private final CardKeyAllocationSupport allocation;
    private final ECouponOrderLifecycleSupport lifecycle;
    private final GiftFulfillmentAlertPublisher giftAlerts;
    private final OrderLogService orderLogs;

    /**
     * 锁定电子卡券订单并按促销类型履约；普通购买及满赠分配卡密，拼团等待成团交付。
     * 非电子卡券、已完成或支付状态不满足门禁的订单不执行分配。
     *
     * @param orderSn 订单编号
     */
    @Transactional(rollbackFor = Exception.class)
    public void fulfillAfterPayment(String orderSn) {
        Order order = orders.selectForUpdateBySn(orderSn);
        if (order == null) {
            log.warn("电子卡券支付后履约跳过，订单不存在，orderSn={}", orderSn);
            return;
        }
        if (!OrderTypeEnum.E_COUPON.name().equals(order.getOrderType())) {
            log.warn("电子卡券支付后履约跳过，非 E_COUPON 订单，orderSn={}，orderType={}", orderSn, order.getOrderType());
            return;
        }
        if (OrderStatusEnum.COMPLETED.name().equals(order.getOrderStatus())) {
            log.info("电子卡券支付后履约跳过，订单已完成，orderSn={}", orderSn);
            return;
        }
        if (!CardKeyTradeSupport.isECouponFulfillmentEligible(order)) {
            log.warn("电子卡券支付后履约拒绝，状态不满足门禁，orderSn={}，payStatus={}，orderStatus={}",
                    orderSn, order.getPayStatus(), order.getOrderStatus());
            return;
        }
        log.info("电子卡券支付后履约开始，orderSn={}，orderStatus={}，promotionType={}",
                orderSn, order.getOrderStatus(), order.getOrderPromotionType());
        if (CardKeyTradeSupport.isGiftECouponOrder(order)) {
            fulfillGift(order);
        } else if (CardKeyTradeSupport.isPintuanECouponOrder(order)) {
            log.info("电子卡券拼团订单等待成团，orderSn={}", orderSn);
        } else if (CardKeyTradeSupport.isPurchaseTypeOrder(order)) {
            fulfillPurchase(orderSn);
        } else {
            log.error("电子卡券支付后履约拒绝，未知促销类型，orderSn={}，promotionType={}",
                    orderSn, order.getOrderPromotionType());
        }
    }

    /**
     * 为已通过状态门禁的购买型订单分配卡密；失败作废并取消退款。
     * 完成订单条件更新未生效时仅记录并返回，不误判为分配失败。
     *
     * @param orderSn 已锁定的订单编号
     */
    private void fulfillPurchase(String orderSn) {
        try {
            allocation.allocateByOrder(orderSn);
            if (!lifecycle.complete(orderSn)) {
                log.warn("电子卡券支付后履约完成 CAS 未生效，orderSn={}", orderSn);
                return;
            }
            log.info("电子卡券支付后履约完成，orderSn={}", orderSn);
        } catch (Exception exception) {
            log.error("电子卡券支付后履约失败，orderSn={}", orderSn, exception);
            allocation.voidOnSystemFailure(orderSn);
            allocation.syncOrderStock(orderSn);
            lifecycle.cancel(orderSn, "库存不足，出库失败", true);
        }
    }

    /**
     * 满赠子单分配失败时作废卡密、取消子单并记录主子单日志和告警，不退款主单。
     *
     * @param giftOrder 已锁定且通过支付状态门禁的满赠子单
     */
    private void fulfillGift(Order giftOrder) {
        String orderSn = giftOrder.getSn();
        Order parentOrder = CharSequenceUtil.isBlank(giftOrder.getParentOrderSn()) ? null
                : orders.selectOne(new LambdaQueryWrapper<Order>().eq(Order::getSn, giftOrder.getParentOrderSn()));
        try {
            allocation.allocateGiftByOrder(orderSn);
            if (!lifecycle.complete(orderSn)) {
                log.warn("满赠电子卡券子单履约完成 CAS 未生效，orderSn={}", orderSn);
                return;
            }
            log.info("满赠电子卡券子单履约完成，orderSn={}", orderSn);
        } catch (Exception exception) {
            log.error("满赠电子卡券子单履约失败，orderSn={}", orderSn, exception);
            allocation.voidOnSystemFailure(orderSn);
            allocation.syncOrderStock(orderSn);
            lifecycle.cancel(orderSn, "满赠电子卡券发放失败", false);
            giftAlerts.onGiftFulfillmentFailure(giftOrder, parentOrder, exception.getMessage());
            writeGiftFailureLogs(giftOrder, parentOrder, exception.getMessage());
        }
    }

    /**
     * 拼团付款后预占卡密，订单状态及重复预占检查由卡密组件在订单锁内执行。
     *
     * @param orderSn 拼团订单编号
     */
    @Transactional(rollbackFor = Exception.class)
    public void reserveByOrder(String orderSn) {
        allocation.reserveByOrder(orderSn);
    }

    /**
     * 成团后确认预占并完成订单；已有已分配卡密时允许重试完成，无卡密时跳过。
     *
     * @param orderSn 拼团订单编号
     */
    @Transactional(rollbackFor = Exception.class)
    public void confirmReservationAndComplete(String orderSn) {
        if (allocation.confirmReservation(orderSn)) {
            lifecycle.complete(orderSn);
            log.info("拼团电子卡券预占确认完成，orderSn={}", orderSn);
        }
    }

    /**
     * 释放拼团预占，保持与后续取消退款的原有调用顺序；本方法不自行取消订单。
     *
     * @param orderSn 拼团订单编号
     */
    @Transactional(rollbackFor = Exception.class)
    public void releaseReservation(String orderSn) {
        allocation.releaseReservation(orderSn);
    }

    /**
     * 保存赠品子单失败原因，并在主单记录赠品失败及主单不受影响的说明。
     *
     * @param giftOrder 满赠子单
     * @param parentOrder 主订单，允许为空
     * @param reason 失败原因
     */
    private void writeGiftFailureLogs(Order giftOrder, Order parentOrder, String reason) {
        String detail = CharSequenceUtil.blankToDefault(reason, "卡池库存不足");
        orderLogs.save(new OrderLog(giftOrder.getSn(), "-1", UserEnums.SYSTEM.name(), UserEnums.SYSTEM.getRole(),
                "满赠电子卡券发卡失败，赠品子单已取消。原因：" + detail));
        if (parentOrder != null) {
            orderLogs.save(new OrderLog(parentOrder.getSn(), "-1", UserEnums.SYSTEM.name(), UserEnums.SYSTEM.getRole(),
                    "满赠赠品（子单 " + giftOrder.getSn() + "）卡密发放失败，主订单不受影响"));
        }
    }
}
