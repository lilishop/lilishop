package cn.lili.modules.goods.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.goods.entity.dos.CardKey;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.enums.CardKeyStatusEnum;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.mapper.CardKeyMapper;
import cn.lili.modules.goods.service.GiftFulfillmentAlertPublisher;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.mapper.OrderMapper;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.trade.entity.dos.OrderLog;
import cn.lili.modules.order.trade.service.OrderLogService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import lombok.extern.slf4j.Slf4j;

import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * 电子卡券卡密支付后履约、预占、成团交付、释放与系统失败回滚执行器
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Slf4j
public class CardKeyFulfillExecutor {

    private static final String FULFILL_FAIL_CANCEL_REASON = "库存不足，出库失败";
    private static final String GIFT_FULFILL_FAIL_CANCEL_REASON = "满赠电子卡券发放失败";

    /**
     * 电子卡券订单支付后履约统一入口。
     *
     * @param service 卡密服务实现（用于反射与模拟回调）
     * @param orderMapper 订单持久层映射
     * @param orderService 订单业务服务
     * @param orderItemService 订单项服务
     * @param orderLogService 订单日志服务
     * @param giftFulfillmentAlertPublisher 满赠失败告警通知器
     * @param orderSn 待履约订单号
     */
    public static void fulfillAfterPayment(CardKeyServiceImpl service,
                                          OrderMapper orderMapper,
                                          OrderService orderService,
                                          OrderItemService orderItemService,
                                          OrderLogService orderLogService,
                                          GiftFulfillmentAlertPublisher giftFulfillmentAlertPublisher,
                                          String orderSn) {
        Order order = orderMapper.selectForUpdateBySn(orderSn);
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
        if (!service.isECouponFulfillmentEligible(order)) {
            log.warn("电子卡券支付后履约拒绝，状态不满足门禁，orderSn={}，payStatus={}，orderStatus={}",
                    orderSn, order.getPayStatus(), order.getOrderStatus());
            return;
        }
        log.info("电子卡券支付后履约开始，orderSn={}，orderStatus={}，promotionType={}",
                orderSn, order.getOrderStatus(), order.getOrderPromotionType());
        if (service.isGiftECouponOrder(order)) {
            fulfillGiftECouponOrder(service, orderMapper, orderService, orderItemService, orderLogService, giftFulfillmentAlertPublisher, order);
        } else if (service.isPintuanECouponOrder(order)) {
            log.info("电子卡券拼团订单等待成团，orderSn={}", orderSn);
        } else if (service.isPurchaseTypeOrder(order)) {
            fulfillPurchaseECouponOrder(service, orderService, orderItemService, order);
        } else {
            log.error("电子卡券支付后履约拒绝，未知促销类型，orderSn={}，promotionType={}",
                    orderSn, order.getOrderPromotionType());
        }
    }

    /**
     * 购买型 E_COUPON：分配卡密后通过 {@link OrderService#systemCompleteECoupon} 完成订单；失败则作废并取消。
     */
    public static void fulfillPurchaseECouponOrder(CardKeyServiceImpl service,
                                                  OrderService orderService,
                                                  OrderItemService orderItemService,
                                                  Order order) {
        String orderSn = order.getSn();
        try {
            service.allocateByOrder(orderSn);
            if (!orderService.systemCompleteECoupon(orderSn)) {
                log.warn("电子卡券支付后履约完成 CAS 未生效，orderSn={}", orderSn);
                return;
            }
            log.info("电子卡券支付后履约完成，orderSn={}", orderSn);
        } catch (Exception e) {
            log.error("电子卡券支付后履约失败，orderSn={}", orderSn, e);
            service.voidOnSystemFailure(orderSn);
            orderItemService.getByOrderSn(orderSn)
                    .forEach(item -> service.syncSkuStock(item.getSkuId()));
            orderService.systemCancelECoupon(orderSn, FULFILL_FAIL_CANCEL_REASON, true);
        }
    }

    /**
     * 满赠 E_COUPON 子单：分配卡密后完成子单；失败则作废、取消子单并告警主单。
     */
    public static void fulfillGiftECouponOrder(CardKeyServiceImpl service,
                                              OrderMapper orderMapper,
                                              OrderService orderService,
                                              OrderItemService orderItemService,
                                              OrderLogService orderLogService,
                                              GiftFulfillmentAlertPublisher giftFulfillmentAlertPublisher,
                                              Order giftOrder) {
        String orderSn = giftOrder.getSn();
        Order parentOrder = CharSequenceUtil.isBlank(giftOrder.getParentOrderSn()) ? null
                : orderMapper.selectOne(new LambdaQueryWrapper<Order>().eq(Order::getSn, giftOrder.getParentOrderSn()));
        try {
            allocateGiftByOrder(service, (CardKeyMapper) service.getBaseMapper(), orderMapper, orderItemService, service.getGoodsSkuService(), orderSn);
            if (!orderService.systemCompleteECoupon(orderSn)) {
                log.warn("满赠电子卡券子单履约完成 CAS 未生效，orderSn={}", orderSn);
                return;
            }
            log.info("满赠电子卡券子单履约完成，orderSn={}", orderSn);
        } catch (Exception e) {
            log.error("满赠电子卡券子单履约失败，orderSn={}", orderSn, e);
            service.voidOnSystemFailure(orderSn);
            orderItemService.getByOrderSn(orderSn)
                    .forEach(item -> service.syncSkuStock(item.getSkuId()));
            orderService.systemCancelECoupon(orderSn, GIFT_FULFILL_FAIL_CANCEL_REASON, false);
            giftFulfillmentAlertPublisher.onGiftFulfillmentFailure(giftOrder, parentOrder, e.getMessage());
            writeGiftFulfillmentFailureLogs(orderLogService, giftOrder, parentOrder, e.getMessage());
        }
    }

    /**
     * 写入满赠卡密履约失败相关订单日志。
     */
    public static void writeGiftFulfillmentFailureLogs(OrderLogService orderLogService,
                                                       Order giftOrder,
                                                       Order parentOrder,
                                                       String reason) {
        String detail = CharSequenceUtil.blankToDefault(reason, "卡池库存不足");
        OrderLog giftLog = new OrderLog(
                giftOrder.getSn(),
                "-1",
                UserEnums.SYSTEM.name(),
                UserEnums.SYSTEM.getRole(),
                "满赠电子卡券发卡失败，赠品子单已取消。原因：" + detail);
        orderLogService.save(giftLog);
        if (parentOrder != null) {
            OrderLog parentLog = new OrderLog(
                    parentOrder.getSn(),
                    "-1",
                    UserEnums.SYSTEM.name(),
                    UserEnums.SYSTEM.getRole(),
                    "满赠赠品（子单 " + giftOrder.getSn() + "）卡密发放失败，主订单不受影响");
            orderLogService.save(parentLog);
        }
    }

    /**
     * 按订单扣减/分配卡密。
     */
    public static void allocateByOrder(CardKeyServiceImpl service,
                                       CardKeyMapper baseMapper,
                                       OrderMapper orderMapper,
                                       OrderItemService orderItemService,
                                       GoodsSkuService goodsSkuService,
                                       String orderSn) {
        Order order = orderMapper.selectForUpdateBySn(orderSn);
        if (order == null || !service.isPurchaseTypeOrder(order)) {
            return;
        }
        if (service.alreadyBound(orderSn, CardKeyStatusEnum.ALLOCATED)) {
            return;
        }
        bindCardsForOrder(service, baseMapper, orderItemService, goodsSkuService, order, orderSn, CardKeyStatusEnum.ALLOCATED);
    }

    /**
     * 拼团订单付款后预占卡密（RESERVED）。
     */
    public static void reserveByOrder(CardKeyServiceImpl service,
                                      CardKeyMapper baseMapper,
                                      OrderMapper orderMapper,
                                      OrderItemService orderItemService,
                                      GoodsSkuService goodsSkuService,
                                      String orderSn) {
        Order order = orderMapper.selectForUpdateBySn(orderSn);
        if (order == null || !service.isPintuanECouponOrder(order)) {
            return;
        }
        if (!service.isECouponFulfillmentEligible(order)) {
            log.warn("电子卡券拼团预占拒绝，状态不满足门禁，orderSn={}，payStatus={}，orderStatus={}",
                    orderSn, order.getPayStatus(), order.getOrderStatus());
            return;
        }
        if (service.alreadyBound(orderSn, CardKeyStatusEnum.RESERVED)
                || service.alreadyBound(orderSn, CardKeyStatusEnum.ALLOCATED)) {
            return;
        }
        bindCardsForOrder(service, baseMapper, orderItemService, goodsSkuService, order, orderSn, CardKeyStatusEnum.RESERVED);
    }

    /**
     * 拼团成团后确认预占（RESERVED -> ALLOCATED）并完成订单。
     */
    public static void confirmReservationAndComplete(CardKeyServiceImpl service,
                                                     CardKeyMapper baseMapper,
                                                     OrderMapper orderMapper,
                                                     OrderService orderService,
                                                     String orderSn) {
        Order order = orderMapper.selectForUpdateBySn(orderSn);
        if (order == null || !service.isPintuanECouponOrder(order)) {
            return;
        }
        List<CardKey> reserved = baseMapper.selectReservedByOrder(orderSn);
        if (reserved.isEmpty()) {
            if (service.alreadyBound(orderSn, CardKeyStatusEnum.ALLOCATED)) {
                orderService.systemCompleteECoupon(orderSn);
            }
            return;
        }
        Date now = new Date();
        Set<String> skuIds = new HashSet<>();
        for (CardKey card : reserved) {
            card.setStatus(CardKeyStatusEnum.ALLOCATED.name());
            card.setAllocatedTime(now);
            service.updateById(card);
            skuIds.add(card.getSkuId());
        }
        skuIds.forEach(service::syncSkuStock);
        orderService.systemCompleteECoupon(orderSn);
        log.info("拼团电子卡券预占确认完成，orderSn={}", orderSn);
    }

    /**
     * 拼团超时未成团释放已预占卡密（RESERVED -> UNUSED）。
     */
    public static void releaseReservation(CardKeyServiceImpl service,
                                          CardKeyMapper baseMapper,
                                          OrderMapper orderMapper,
                                          String orderSn) {
        orderMapper.selectForUpdateBySn(orderSn);
        List<CardKey> reserved = baseMapper.selectReservedByOrder(orderSn);
        if (reserved.isEmpty()) {
            return;
        }
        Set<String> skuIds = new HashSet<>();
        for (CardKey cardKey : reserved) {
            service.update(new LambdaUpdateWrapper<CardKey>()
                    .eq(CardKey::getId, cardKey.getId())
                    .set(CardKey::getStatus, CardKeyStatusEnum.UNUSED.name())
                    .set(CardKey::getOrderSn, null)
                    .set(CardKey::getOrderItemSn, null)
                    .set(CardKey::getMemberId, null)
                    .set(CardKey::getAllocatedTime, null));
            skuIds.add(cardKey.getSkuId());
        }
        skuIds.forEach(service::syncSkuStock);
        log.info("拼团电子卡券预占已释放，orderSn={}", orderSn);
    }

    /**
     * 满赠子单分配卡密（调用方已持有订单行锁）。
     */
    public static void allocateGiftByOrder(CardKeyServiceImpl service,
                                           CardKeyMapper baseMapper,
                                           OrderMapper orderMapper,
                                           OrderItemService orderItemService,
                                           GoodsSkuService goodsSkuService,
                                           String orderSn) {
        Order order = orderMapper.selectForUpdateBySn(orderSn);
        if (order == null || !service.isGiftECouponOrder(order)) {
            return;
        }
        if (service.alreadyBound(orderSn, CardKeyStatusEnum.ALLOCATED)) {
            return;
        }
        bindCardsForOrder(service, baseMapper, orderItemService, goodsSkuService, order, orderSn, CardKeyStatusEnum.ALLOCATED);
    }

    /**
     * 按订单明细从卡池绑定卡密；购买型 ALLOCATED 失败时触发 {@link #voidOnSystemFailure}。
     */
    public static void bindCardsForOrder(CardKeyServiceImpl service,
                                         CardKeyMapper baseMapper,
                                         OrderItemService orderItemService,
                                         GoodsSkuService goodsSkuService,
                                         Order order,
                                         String orderSn,
                                         CardKeyStatusEnum targetStatus) {
        if (order == null) {
            return;
        }
        List<OrderItem> orderItems = orderItemService.getByOrderSn(orderSn);
        try {
            for (OrderItem orderItem : orderItems) {
                GoodsSku sku = goodsSkuService.getById(orderItem.getSkuId());
                if (sku == null || !GoodsTypeEnum.E_COUPON.name().equals(sku.getGoodsType())) {
                    continue;
                }
                List<CardKey> cards = baseMapper.selectForAllocate(orderItem.getSkuId(), orderItem.getNum());
                if (cards.size() < orderItem.getNum()) {
                    throw new ServiceException(ResultCode.CARD_KEY_POOL_INSUFFICIENT);
                }
                Date now = new Date();
                for (CardKey card : cards) {
                    card.setStatus(targetStatus.name());
                    card.setOrderSn(orderSn);
                    card.setOrderItemSn(orderItem.getSn());
                    card.setMemberId(order.getMemberId());
                    if (CardKeyStatusEnum.ALLOCATED.equals(targetStatus)) {
                        card.setAllocatedTime(now);
                    }
                    service.updateById(card);
                }
                service.syncSkuStock(orderItem.getSkuId());
            }
        } catch (RuntimeException ex) {
            if (CardKeyStatusEnum.ALLOCATED.equals(targetStatus) && service.isPurchaseTypeOrder(order)) {
                service.voidOnSystemFailure(orderSn);
            }
            throw ex;
        }
    }

    /**
     * 履约系统失败时将当前订单已分配的卡密作废（防止已生成卡密被重复泄露或发放）。
     */
    public static void voidOnSystemFailure(CardKeyServiceImpl service, String orderSn) {
        List<CardKey> allocated = service.list(new LambdaQueryWrapper<CardKey>()
                .eq(CardKey::getOrderSn, orderSn)
                .eq(CardKey::getStatus, CardKeyStatusEnum.ALLOCATED.name())
                .eq(CardKey::getDeleteFlag, false));
        if (allocated.isEmpty()) {
            return;
        }
        Set<String> skuIds = new HashSet<>();
        for (CardKey cardKey : allocated) {
            service.update(new LambdaUpdateWrapper<CardKey>()
                    .eq(CardKey::getId, cardKey.getId())
                    .set(CardKey::getStatus, CardKeyStatusEnum.VOIDED.name())
                    .set(CardKey::getOrderSn, null)
                    .set(CardKey::getOrderItemSn, null)
                    .set(CardKey::getMemberId, null)
                    .set(CardKey::getAllocatedTime, null));
            skuIds.add(cardKey.getSkuId());
        }
        skuIds.forEach(service::syncSkuStock);
    }
}
