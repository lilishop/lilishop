package cn.lili.modules.distribution.serviceimpl;

import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionGoods;
import cn.lili.modules.distribution.entity.dos.DistributionGrade;
import cn.lili.modules.distribution.entity.dos.DistributionOrder;
import cn.lili.modules.distribution.entity.enums.DistributionOrderStatusEnum;
import cn.lili.modules.distribution.entity.enums.DistributionStatusEnum;
import cn.lili.modules.distribution.service.DistributionCustomerRelationService;
import cn.lili.modules.distribution.service.DistributionGradeService;
import cn.lili.modules.distribution.service.DistributionRewardService;
import cn.lili.modules.distribution.service.DistributionService;
import cn.lili.modules.distribution.support.DistributionOrderPersistSupport;
import cn.lili.modules.distribution.support.DistributionRewardContextSupport;
import cn.lili.modules.distribution.support.DistributionRewardWalletSupport;
import cn.lili.modules.distribution.util.DistributionCommissionCalculator;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.promotion.service.PickupCardOrderService;
import cn.lili.modules.system.entity.dto.DistributionSettlementSetting;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * 分销佣金奖励服务实现，协调佣金计算、分销订单持久化、钱包账务和等级指标更新。
 * 支持类为必需依赖；支付入账、结算及退款沿用本服务事务，持久化支持类不回调分销订单服务。
 */
@Slf4j
@Service
public class DistributionRewardServiceImpl implements DistributionRewardService {

    @Autowired
    private PickupCardOrderService pickupCardOrderService;
    @Autowired
    private OrderItemService orderItemService;
    @Autowired
    private DistributionService distributionService;
    @Autowired
    private DistributionGradeService distributionGradeService;
    @Autowired
    private DistributionCustomerRelationService customerRelationService;
    @Autowired
    private DistributionRewardWalletSupport walletSupport;
    @Autowired
    private DistributionRewardContextSupport contextSupport;
    @Autowired
    private DistributionOrderPersistSupport persistSupport;

    /**
     * 按已支付订单发放分销奖励，冻结佣金并累计有效销售额。
     * 分销订单唯一键和钱包幂等键防止重复入账，全部写入参与当前事务，提货卡订单跳过。
     *
     * @param orderSn 主订单编号
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void handlePaymentRewards(String orderSn) {
        Order order = persistSupport.getOrderBySn(orderSn);
        if (order == null || !PayStatusEnum.PAID.name().equals(order.getPayStatus()) || pickupCardOrderService.isPickupOrder(order.getSn())) {
            return;
        }
        // 支付成功后将客户与分销员绑定为永久关系
        customerRelationService.bindPermanentOnPaid(order.getMemberId(), orderSn);
        List<OrderItem> orderItems = orderItemService.getByOrderSn(orderSn);
        if (orderItems == null || orderItems.isEmpty()) {
            return;
        }

        DistributionSettlementSetting settlementSetting = contextSupport.getSettlementSetting();
        Distribution buyerDistribution = distributionService.getOne(new LambdaQueryWrapper<Distribution>()
                .eq(Distribution::getMemberId, order.getMemberId())
                .eq(Distribution::getDistributionStatus, DistributionStatusEnum.PASS.name()));
        boolean buyerIsDistributor = buyerDistribution != null;

        // 预加载分销商品、已有分销订单及受益人缓存，避免循环内重复查库
        Map<String, DistributionGoods> goodsBySkuId = contextSupport.loadDistributionGoodsMap(orderItems);
        Set<String> existingDistributionOrderItems = persistSupport.loadExistingDistributionOrderItemSns(orderSn);
        Map<String, DistributionOrder> distributionOrderByItem = persistSupport.loadDistributionOrdersByOrderSn(orderSn);
        Map<String, Distribution> distributionCache = new HashMap<>();
        Map<String, DistributionGrade> gradeCache = new HashMap<>();

        // 按自购/推广模式解析直推与邀请受益人
        DistributionRewardContextSupport.CommissionContext commissionContext = contextSupport.resolveCommissionContext(
                order, settlementSetting, buyerDistribution, buyerIsDistributor, distributionCache, gradeCache);

        Map<String, BigDecimal> performanceSalesDelta = new HashMap<>();
        Set<String> performanceDistributionIds = new HashSet<>();

        for (OrderItem orderItem : orderItems) {
            boolean trackPerformance = !existingDistributionOrderItems.contains(orderItem.getSn());
            DistributionGoods distributionGoods = goodsBySkuId.get(orderItem.getSkuId());
            if (distributionGoods == null
                    || !DistributionGoodsServiceImpl.isApproved(distributionGoods)
                    || distributionGoods.getCommission() == null
                    || distributionGoods.getCommission() <= 0) {
                continue;
            }
            BigDecimal pool = DistributionCommissionCalculator.calculatePool(
                    BigDecimal.valueOf(distributionGoods.getCommission()), orderItem.getNum());
            if (pool.compareTo(BigDecimal.ZERO) <= 0) {
                continue;
            }
            if (!commissionContext.hasDirectReward() && !commissionContext.hasInviteReward()) {
                continue;
            }

            BigDecimal directRate = commissionContext.hasDirectReward()
                    ? commissionContext.directGrade.getGoodsCommissionRate() : BigDecimal.ZERO;
            BigDecimal inviteRate = commissionContext.hasInviteReward()
                    ? commissionContext.inviteGrade.getInviteCommissionRate() : BigDecimal.ZERO;
            BigDecimal[] split = DistributionCommissionCalculator.splitPool(pool, directRate, inviteRate);

            DistributionOrderPersistSupport.WritePayload payload = new DistributionOrderPersistSupport.WritePayload();
            payload.distributionId = commissionContext.performanceDistributionId;
            payload.inviteDistributionId = commissionContext.hasInviteReward()
                    ? commissionContext.inviteDistribution.getId() : null;
            payload.inviteDistributionName = commissionContext.hasInviteReward()
                    ? commissionContext.inviteDistribution.getMemberName() : null;
            payload.directCommission = commissionContext.hasDirectReward() ? split[0] : BigDecimal.ZERO;
            payload.inviteCommission = commissionContext.hasInviteReward() ? split[1] : BigDecimal.ZERO;
            payload.commissionPool = pool;

            DistributionOrder distributionOrder = persistSupport.ensureDistributionOrder(
                    order, orderItem, commissionContext, payload, contextSupport,
                    distributionCache, existingDistributionOrderItems, distributionOrderByItem);
            if (distributionOrder == null) {
                continue;
            }
            walletSupport.freezeCommissions(distributionOrder);
            if (trackPerformance) {
                contextSupport.accumulatePerformanceSales(performanceSalesDelta, performanceDistributionIds,
                        commissionContext.performanceDistributionId, orderItem);
            }
        }
        // 批量刷新有效销售额并触发等级重算
        contextSupport.flushPerformanceSales(performanceSalesDelta, performanceDistributionIds);
    }

    /**
     * 结算未完成的分销订单，将剩余冻结佣金转为可用佣金并刷新等级指标。
     * 钱包入账和分销订单更新时间参与同一事务，非待结算状态不重复处理。
     *
     * @param orderItemSn 订单明细编号
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void settleRewardsByOrderItem(String orderItemSn) {
        DistributionOrder order = persistSupport.getDistributionOrderByItemSn(orderItemSn);
        if (order == null || !DistributionOrderStatusEnum.NO_COMPLETED.name().equals(order.getDistributionOrderStatus())) {
            return;
        }
        Date now = new Date();
        Map<String, BigDecimal> commissionDelta = new HashMap<>();
        Set<String> gradeRefreshIds = new HashSet<>();

        walletSupport.settleCommissions(order, commissionDelta, gradeRefreshIds);

        order.setSettleTime(now);
        persistSupport.updateDistributionOrderById(order);

        for (Map.Entry<String, BigDecimal> entry : commissionDelta.entrySet()) {
            contextSupport.addCommissionMetrics(entry.getKey(), entry.getValue());
        }
        gradeRefreshIds.forEach(distributionGradeService::refreshGrade);
    }

    /**
     * 按分销订单状态冲回冻结或已结算佣金，已退款记录不重复处理。
     * 钱包冲回和退款状态更新参与同一事务。
     *
     * @param orderItemSn 订单明细编号
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void refundRewardsByOrderItem(String orderItemSn) {
        DistributionOrder order = persistSupport.getDistributionOrderByItemSn(orderItemSn);
        if (order == null || DistributionOrderStatusEnum.REFUND.name().equals(order.getDistributionOrderStatus())) {
            return;
        }
        boolean wasSettled = DistributionOrderStatusEnum.COMPLETE.name().equals(order.getDistributionOrderStatus());
        walletSupport.refundCommissions(order, wasSettled);
        order.setDistributionOrderStatus(DistributionOrderStatusEnum.REFUND.name());
        persistSupport.updateDistributionOrderById(order);
    }

    /**
     * 取消主订单下未结算的分销奖励，冲回冻结佣金并标记退款，已结算记录不处理。
     * 全部明细与钱包写入参与同一事务，空白订单编号不处理。
     *
     * @param orderSn 主订单编号
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void cancelRewardsByOrderSn(String orderSn) {
        if (orderSn == null || orderSn.isBlank()) {
            return;
        }
        List<DistributionOrder> orders = persistSupport.listDistributionOrders(
                new LambdaQueryWrapper<DistributionOrder>()
                        .eq(DistributionOrder::getOrderSn, orderSn)
                        .eq(DistributionOrder::getDistributionOrderStatus, DistributionOrderStatusEnum.NO_COMPLETED.name()));
        for (DistributionOrder order : orders) {
            walletSupport.refundCommissions(order, false);
            order.setDistributionOrderStatus(DistributionOrderStatusEnum.REFUND.name());
            persistSupport.updateDistributionOrderById(order);
        }
    }

    /**
     * 查询主订单是否已有分销奖励记录，空白编号返回false。
     *
     * @param orderSn 主订单编号
     * @return 是否存在分销订单，不按结算状态过滤
     */
    @Override
    public boolean hasRewardsByOrderSn(String orderSn) {
        if (orderSn == null || orderSn.isBlank()) {
            return false;
        }
        return persistSupport.countDistributionOrders(
                new LambdaQueryWrapper<DistributionOrder>().eq(DistributionOrder::getOrderSn, orderSn)) > 0;
    }

    /**
     * 查询订单明细是否已有分销奖励记录。
     *
     * @param orderItemSn 订单明细编号
     * @return 是否存在分销订单，空白编号返回false
     */
    @Override
    public boolean hasRewardsByOrderItemSn(String orderItemSn) {
        return persistSupport.getDistributionOrderByItemSn(orderItemSn) != null;
    }

}
