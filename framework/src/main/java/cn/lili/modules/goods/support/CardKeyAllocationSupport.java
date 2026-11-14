package cn.lili.modules.goods.support;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.goods.entity.dos.CardKey;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.enums.CardKeyStatusEnum;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.mapper.CardKeyMapper;
import cn.lili.modules.goods.mapper.GoodsSkuMapper;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.mapper.OrderItemMapper;
import cn.lili.modules.order.order.mapper.OrderMapper;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * 电子卡券的分配、拼团预占确认与释放及失败作废能力，不负责订单状态、退款或履约编排。
 * 调用方必须开启事务；订单行锁与卡池行锁持续到调用方事务结束。
 * 分配异常由调用方捕获并处理，不在本组件增加事务代理，以免提前标记调用方事务回滚。
 *
 * @author Bulbasaur
 * @since 2026-11-14
 */
@Component
@Slf4j
@RequiredArgsConstructor
public class CardKeyAllocationSupport {

    private final CardKeyMapper cards;
    private final OrderMapper orders;
    private final OrderItemMapper items;
    private final GoodsSkuMapper skus;
    private final CardKeyPoolSupport pool;

    /**
     * 锁定购买型订单并按创建时间从卡池分配卡密，已绑定卡密时跳过重复分配。
     * 保留已有规则：缺失或非电子卡券SKU跳过，库存不足时作废本次订单已分配卡密后抛出异常。
     *
     * @param orderSn 订单编号
     */
    public void allocateByOrder(String orderSn) {
        Order order = orders.selectForUpdateBySn(orderSn);
        if (!CardKeyTradeSupport.isPurchaseTypeOrder(order) || alreadyBound(orderSn, CardKeyStatusEnum.ALLOCATED)) {
            return;
        }
        try {
            bindCards(order, CardKeyStatusEnum.ALLOCATED);
        } catch (RuntimeException exception) {
            voidOnSystemFailure(orderSn);
            throw exception;
        }
    }

    /**
     * 锁定满赠子单并分配卡密，重复请求不重复分配；失败由上层统一作废、取消及告警。
     *
     * @param orderSn 满赠子单编号
     */
    public void allocateGiftByOrder(String orderSn) {
        Order order = orders.selectForUpdateBySn(orderSn);
        if (CardKeyTradeSupport.isGiftECouponOrder(order) && !alreadyBound(orderSn, CardKeyStatusEnum.ALLOCATED)) {
            bindCards(order, CardKeyStatusEnum.ALLOCATED);
        }
    }

    /**
     * 已支付且待交付的拼团订单预占卡密；已有预占或已分配卡密时跳过，防止重复占用。
     *
     * @param orderSn 拼团订单编号
     */
    public void reserveByOrder(String orderSn) {
        Order order = orders.selectForUpdateBySn(orderSn);
        if (!CardKeyTradeSupport.isPintuanECouponOrder(order)) {
            return;
        }
        if (!CardKeyTradeSupport.isECouponFulfillmentEligible(order)) {
            log.warn("电子卡券拼团预占拒绝，状态不满足门禁，orderSn={}，payStatus={}，orderStatus={}",
                    orderSn, order.getPayStatus(), order.getOrderStatus());
            return;
        }
        if (!alreadyBound(orderSn, CardKeyStatusEnum.RESERVED) && !alreadyBound(orderSn, CardKeyStatusEnum.ALLOCATED)) {
            bindCards(order, CardKeyStatusEnum.RESERVED);
        }
    }

    /**
     * 锁定拼团订单，将预占卡密确认分配并同步库存；不在本组件完成订单。
     *
     * @param orderSn 拼团订单编号
     * @return 本次确认或此前已有已分配卡密为true，无可确认卡密或非拼团订单为false
     */
    public boolean confirmReservation(String orderSn) {
        Order order = orders.selectForUpdateBySn(orderSn);
        if (!CardKeyTradeSupport.isPintuanECouponOrder(order)) {
            return false;
        }
        List<CardKey> reserved = cards.selectReservedByOrder(orderSn);
        if (reserved.isEmpty()) {
            return alreadyBound(orderSn, CardKeyStatusEnum.ALLOCATED);
        }
        Date now = new Date();
        Set<String> skuIds = new HashSet<>();
        for (CardKey card : reserved) {
            card.setStatus(CardKeyStatusEnum.ALLOCATED.name());
            card.setAllocatedTime(now);
            cards.updateById(card);
            skuIds.add(card.getSkuId());
        }
        skuIds.forEach(pool::syncSkuStock);
        return true;
    }

    /**
     * 锁定订单并将预占卡密释放为未使用，清除绑定后同步库存；不释放已分配卡密。
     *
     * @param orderSn 订单编号
     */
    public void releaseReservation(String orderSn) {
        orders.selectForUpdateBySn(orderSn);
        List<CardKey> reserved = cards.selectReservedByOrder(orderSn);
        if (reserved.isEmpty()) {
            return;
        }
        Set<String> skuIds = new HashSet<>();
        for (CardKey card : reserved) {
            cards.update(null, new LambdaUpdateWrapper<CardKey>()
                    .eq(CardKey::getId, card.getId())
                    .set(CardKey::getStatus, CardKeyStatusEnum.UNUSED.name())
                    .set(CardKey::getOrderSn, null)
                    .set(CardKey::getOrderItemSn, null)
                    .set(CardKey::getMemberId, null)
                    .set(CardKey::getAllocatedTime, null));
            skuIds.add(card.getSkuId());
        }
        skuIds.forEach(pool::syncSkuStock);
        log.info("拼团电子卡券预占已释放，orderSn={}", orderSn);
    }

    /**
     * 查询订单是否已有指定状态的有效绑定，调用方持有订单行锁以串行化分配与预占。
     *
     * @param orderSn 订单编号
     * @param status 待检查的绑定状态
     * @return 是否存在绑定
     */
    private boolean alreadyBound(String orderSn, CardKeyStatusEnum status) {
        return cards.selectCount(new LambdaQueryWrapper<CardKey>()
                .eq(CardKey::getOrderSn, orderSn).eq(CardKey::getStatus, status.name())
                .eq(CardKey::getDeleteFlag, false)) > 0;
    }

    /**
     * 按订单明细FIFO锁定卡池并绑定到订单；库存不足抛出异常，分支对应的失败策略由调用方执行。
     *
     * @param order 已锁定的订单
     * @param status 目标状态，预占为RESERVED，交付为ALLOCATED
     */
    private void bindCards(Order order, CardKeyStatusEnum status) {
        for (OrderItem item : items.selectList(new LambdaQueryWrapper<OrderItem>().eq(OrderItem::getOrderSn, order.getSn()))) {
            GoodsSku sku = skus.selectById(item.getSkuId());
            if (sku == null || !GoodsTypeEnum.E_COUPON.name().equals(sku.getGoodsType())) {
                continue;
            }
            List<CardKey> available = cards.selectForAllocate(item.getSkuId(), item.getNum());
            if (available.size() < item.getNum()) {
                throw new ServiceException(ResultCode.CARD_KEY_POOL_INSUFFICIENT);
            }
            Date now = new Date();
            for (CardKey card : available) {
                card.setStatus(status.name());
                card.setOrderSn(order.getSn());
                card.setOrderItemSn(item.getSn());
                card.setMemberId(order.getMemberId());
                if (CardKeyStatusEnum.ALLOCATED.equals(status)) {
                    card.setAllocatedTime(now);
                }
                cards.updateById(card);
            }
            pool.syncSkuStock(item.getSkuId());
        }
    }

    /**
     * 作废订单已分配卡密并清除绑定信息，防止失败订单卡密再次出售；重复调用不修改其他状态。
     * 同步受影响SKU库存，参与调用方事务，不独立提交补偿。
     *
     * @param orderSn 订单编号
     */
    public void voidOnSystemFailure(String orderSn) {
        List<CardKey> allocated = cards.selectList(new LambdaQueryWrapper<CardKey>()
                .eq(CardKey::getOrderSn, orderSn)
                .eq(CardKey::getStatus, CardKeyStatusEnum.ALLOCATED.name())
                .eq(CardKey::getDeleteFlag, false));
        Set<String> skuIds = new HashSet<>();
        for (CardKey card : allocated) {
            cards.update(null, new LambdaUpdateWrapper<CardKey>()
                    .eq(CardKey::getId, card.getId())
                    .set(CardKey::getStatus, CardKeyStatusEnum.VOIDED.name())
                    .set(CardKey::getOrderSn, null)
                    .set(CardKey::getOrderItemSn, null)
                    .set(CardKey::getMemberId, null)
                    .set(CardKey::getAllocatedTime, null));
            skuIds.add(card.getSkuId());
        }
        skuIds.forEach(pool::syncSkuStock);
    }

    /**
     * 分配失败后按订单明细重新核对库存，包含尚未成功绑定卡密的SKU。
     *
     * @param orderSn 订单编号
     */
    public void syncOrderStock(String orderSn) {
        items.selectList(new LambdaQueryWrapper<OrderItem>().eq(OrderItem::getOrderSn, orderSn))
                .forEach(item -> pool.syncSkuStock(item.getSkuId()));
    }
}
