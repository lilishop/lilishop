package cn.lili.modules.distribution.serviceimpl;

import cn.hutool.json.JSONUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionCustomerRelation;
import cn.lili.modules.distribution.entity.dos.DistributionGoods;
import cn.lili.modules.distribution.entity.dos.DistributionGrade;
import cn.lili.modules.distribution.entity.dos.DistributionOrder;
import cn.lili.modules.distribution.entity.enums.DistributionOrderStatusEnum;
import cn.lili.modules.distribution.entity.enums.DistributionSelfPurchaseModeEnum;
import cn.lili.modules.distribution.entity.enums.DistributionStatusEnum;
import cn.lili.modules.distribution.service.DistributionCustomerRelationService;
import cn.lili.modules.distribution.service.DistributionGoodsService;
import cn.lili.modules.distribution.service.DistributionGradeService;
import cn.lili.modules.distribution.service.DistributionOrderService;
import cn.lili.modules.distribution.service.DistributionRewardService;
import cn.lili.modules.distribution.service.DistributionService;
import cn.lili.modules.distribution.service.DistributionWalletAccountService;
import cn.lili.modules.distribution.util.DistributionCommissionCalculator;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.DistributionSettlementSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * 分销佣金奖励服务实现（基于 li_distribution_order，不再使用奖励明细表）。
 */
@Slf4j
@Service
public class DistributionRewardServiceImpl implements DistributionRewardService {

    private static final int IN_QUERY_BATCH_SIZE = 500;

    @Autowired
    private OrderService orderService;

    @Autowired
    private OrderItemService orderItemService;

    @Autowired
    @Lazy
    private DistributionOrderService distributionOrderService;

    @Autowired
    private DistributionGoodsService distributionGoodsService;

    @Autowired
    private DistributionService distributionService;

    @Autowired
    private DistributionGradeService distributionGradeService;

    @Autowired
    private DistributionCustomerRelationService customerRelationService;

    @Autowired
    private DistributionWalletAccountService walletAccountService;

    @Autowired
    private SettingService settingService;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void handlePaymentRewards(String orderSn) {
        Order order = orderService.getBySn(orderSn);
        if (order == null || !PayStatusEnum.PAID.name().equals(order.getPayStatus())) {
            return;
        }
        // 支付成功后将客户与分销员绑定为永久关系
        customerRelationService.bindPermanentOnPaid(order.getMemberId(), orderSn);
        List<OrderItem> orderItems = orderItemService.getByOrderSn(orderSn);
        if (orderItems == null || orderItems.isEmpty()) {
            return;
        }

        DistributionSettlementSetting settlementSetting = getSettlementSetting();
        Distribution buyerDistribution = distributionService.getOne(new LambdaQueryWrapper<Distribution>()
                .eq(Distribution::getMemberId, order.getMemberId())
                .eq(Distribution::getDistributionStatus, DistributionStatusEnum.PASS.name()));
        boolean buyerIsDistributor = buyerDistribution != null;

        // 预加载分销商品、已有分销订单及受益人缓存，避免循环内重复查库
        Map<String, DistributionGoods> goodsBySkuId = loadDistributionGoodsMap(orderItems);
        Set<String> existingDistributionOrderItems = loadExistingDistributionOrderItemSns(orderSn);
        Map<String, DistributionOrder> distributionOrderByItem = loadDistributionOrdersByOrderSn(orderSn);
        Map<String, Distribution> distributionCache = new HashMap<>();
        Map<String, DistributionGrade> gradeCache = new HashMap<>();

        // 按自购/推广模式解析直推与邀请受益人
        CommissionContext commissionContext = resolveCommissionContext(
                order, settlementSetting, buyerDistribution, buyerIsDistributor, distributionCache, gradeCache);
        commissionContext.buyerDistributionId = buyerDistribution == null ? null : buyerDistribution.getId();

        Map<String, BigDecimal> performanceSalesDelta = new HashMap<>();
        Set<String> performanceDistributionIds = new HashSet<>();

        for (OrderItem orderItem : orderItems) {
            // 首次写入的分销订单才计入业绩，避免重复支付回调累加
            boolean trackPerformance = !existingDistributionOrderItems.contains(orderItem.getSn());
            DistributionGoods distributionGoods = goodsBySkuId.get(orderItem.getSkuId());
            // 仅已审核且佣金大于 0 的分销商品参与分佣
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
            // 按等级比例将佣金池拆分为直推与邀请两部分
            BigDecimal[] split = DistributionCommissionCalculator.splitPool(pool, directRate, inviteRate);

            String performanceDistributionId = commissionContext.performanceDistributionId;
            BigDecimal directRebate = commissionContext.hasDirectReward() ? split[0] : BigDecimal.ZERO;
            BigDecimal inviteRebate = commissionContext.hasInviteReward() ? split[1] : BigDecimal.ZERO;
            String inviteDistributionId = commissionContext.hasInviteReward()
                    ? commissionContext.inviteDistribution.getId() : null;
            String inviteDistributionName = commissionContext.hasInviteReward()
                    ? commissionContext.inviteDistribution.getMemberName() : null;

            WritePayload payload = new WritePayload();
            payload.distributionId = performanceDistributionId;
            payload.inviteDistributionId = inviteDistributionId;
            payload.inviteDistributionName = inviteDistributionName;
            payload.directCommission = directRebate;
            payload.inviteCommission = inviteRebate;
            payload.commissionPool = pool;

            DistributionOrder distributionOrder = ensureDistributionOrder(
                    order, orderItem, commissionContext, payload,
                    distributionCache, existingDistributionOrderItems, distributionOrderByItem);
            if (distributionOrder == null) {
                continue;
            }
            freezeCommissions(distributionOrder);
            if (trackPerformance) {
                accumulatePerformanceSales(performanceSalesDelta, performanceDistributionIds,
                        performanceDistributionId, orderItem);
            }
        }
        // 批量刷新有效销售额并触发等级重算
        flushPerformanceSales(performanceSalesDelta, performanceDistributionIds);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void settleRewardsByOrderItem(String orderItemSn) {
        DistributionOrder order = getDistributionOrderByItemSn(orderItemSn);
        if (order == null || !DistributionOrderStatusEnum.NO_COMPLETED.name().equals(order.getDistributionOrderStatus())) {
            return;
        }
        Date now = new Date();
        Map<String, BigDecimal> commissionDelta = new HashMap<>();
        Set<String> gradeRefreshIds = new HashSet<>();

        // 直推与邀请佣金分别解冻结算
        settleDirectCommission(order, commissionDelta, gradeRefreshIds);
        settleInviteCommission(order, commissionDelta, gradeRefreshIds);

        order.setSettleTime(now);
        distributionOrderService.updateById(order);

        // 累加分销员有效佣金指标并刷新等级
        for (Map.Entry<String, BigDecimal> entry : commissionDelta.entrySet()) {
            addCommissionMetrics(entry.getKey(), entry.getValue());
        }
        gradeRefreshIds.forEach(distributionGradeService::refreshGrade);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void refundRewardsByOrderItem(String orderItemSn) {
        DistributionOrder order = getDistributionOrderByItemSn(orderItemSn);
        if (order == null || DistributionOrderStatusEnum.REFUND.name().equals(order.getDistributionOrderStatus())) {
            return;
        }
        // 已结算走退款冲回，未结算走冻结取消
        boolean wasSettled = DistributionOrderStatusEnum.COMPLETE.name().equals(order.getDistributionOrderStatus());
        refundDirectCommission(order, wasSettled);
        refundInviteCommission(order, wasSettled);
        order.setDistributionOrderStatus(DistributionOrderStatusEnum.REFUND.name());
        distributionOrderService.updateById(order);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void cancelRewardsByOrderSn(String orderSn) {
        if (orderSn == null || orderSn.isBlank()) {
            return;
        }
        List<DistributionOrder> orders = distributionOrderService.list(new LambdaQueryWrapper<DistributionOrder>()
                .eq(DistributionOrder::getOrderSn, orderSn)
                .eq(DistributionOrder::getDistributionOrderStatus, DistributionOrderStatusEnum.NO_COMPLETED.name()));
        // 整单取消时仅处理仍处于冻结状态的明细
        for (DistributionOrder order : orders) {
            cancelFrozenCommission(order);
        }
    }

    @Override
    public boolean hasRewardsByOrderSn(String orderSn) {
        if (orderSn == null || orderSn.isBlank()) {
            return false;
        }
        return distributionOrderService.count(new LambdaQueryWrapper<DistributionOrder>()
                .eq(DistributionOrder::getOrderSn, orderSn)) > 0;
    }

    @Override
    public boolean hasRewardsByOrderItemSn(String orderItemSn) {
        return getDistributionOrderByItemSn(orderItemSn) != null;
    }

    /**
     * 按订单明细编号查询分销订单。
     *
     * @param orderItemSn 订单明细编号
     * @return 分销订单，不存在或参数为空时返回 null
     */
    private DistributionOrder getDistributionOrderByItemSn(String orderItemSn) {
        if (orderItemSn == null || orderItemSn.isBlank()) {
            return null;
        }
        return distributionOrderService.getOne(new LambdaQueryWrapper<DistributionOrder>()
                .eq(DistributionOrder::getOrderItemSn, orderItemSn)
                .last("LIMIT 1"), false);
    }

    /**
     * 冻结分销订单中的直推与邀请佣金。
     *
     * @param order 分销订单
     */
    private void freezeCommissions(DistributionOrder order) {
        BigDecimal directAmount = directRemain(order);
        if (directAmount.compareTo(BigDecimal.ZERO) > 0 && StringUtils.isNotEmpty(order.getDistributionId())) {
            walletAccountService.freezeCommission(order.getDistributionId(), directAmount, order.getOrderItemSn(),
                    freezeKey(order.getOrderItemSn(), "DIRECT", order.getDistributionId()), "支付奖励冻结");
        }
        BigDecimal inviteAmount = inviteRemain(order);
        if (inviteAmount.compareTo(BigDecimal.ZERO) > 0 && StringUtils.isNotEmpty(order.getInviteDistributionId())) {
            walletAccountService.freezeCommission(order.getInviteDistributionId(), inviteAmount, order.getOrderItemSn(),
                    freezeKey(order.getOrderItemSn(), "INVITE", order.getInviteDistributionId()), "支付邀请奖励冻结");
        }
    }

    /**
     * 结算直推佣金并累计待刷新等级。
     *
     * @param order            分销订单
     * @param commissionDelta  分销员佣金增量汇总
     * @param gradeRefreshIds  待刷新等级的分销员 ID 集合
     */
    private void settleDirectCommission(DistributionOrder order,
                                        Map<String, BigDecimal> commissionDelta,
                                        Set<String> gradeRefreshIds) {
        BigDecimal amount = directRemain(order);
        if (amount.compareTo(BigDecimal.ZERO) <= 0 || StringUtils.isEmpty(order.getDistributionId())) {
            return;
        }
        walletAccountService.settleCommission(order.getDistributionId(), amount, order.getOrderItemSn(),
                settleKey(order.getOrderItemSn(), "DIRECT", order.getDistributionId()), "佣金解冻结算");
        commissionDelta.merge(order.getDistributionId(), amount, BigDecimal::add);
        gradeRefreshIds.add(order.getDistributionId());
    }

    /**
     * 结算邀请佣金并累计待刷新等级。
     *
     * @param order            分销订单
     * @param commissionDelta  分销员佣金增量汇总
     * @param gradeRefreshIds  待刷新等级的分销员 ID 集合
     */
    private void settleInviteCommission(DistributionOrder order,
                                        Map<String, BigDecimal> commissionDelta,
                                        Set<String> gradeRefreshIds) {
        BigDecimal amount = inviteRemain(order);
        if (amount.compareTo(BigDecimal.ZERO) <= 0 || StringUtils.isEmpty(order.getInviteDistributionId())) {
            return;
        }
        walletAccountService.settleCommission(order.getInviteDistributionId(), amount, order.getOrderItemSn(),
                settleKey(order.getOrderItemSn(), "INVITE", order.getInviteDistributionId()), "邀请奖励解冻结算");
        commissionDelta.merge(order.getInviteDistributionId(), amount, BigDecimal::add);
        gradeRefreshIds.add(order.getInviteDistributionId());
    }

    /**
     * 退款冲回直推佣金。
     *
     * @param order       分销订单
     * @param wasSettled  是否已结算
     */
    private void refundDirectCommission(DistributionOrder order, boolean wasSettled) {
        BigDecimal remain = directRemain(order);
        if (remain.compareTo(BigDecimal.ZERO) <= 0 || StringUtils.isEmpty(order.getDistributionId())) {
            return;
        }
        String statusTag = wasSettled ? "SETTLED" : "FROZEN";
        walletAccountService.refundCommission(order.getDistributionId(), remain, order.getOrderItemSn(),
                "order:refund:" + order.getOrderItemSn() + ":DIRECT:" + statusTag, "退款冲回佣金");
        order.setSellBackRebate(defaultDouble(order.getDirectCommission()));
    }

    /**
     * 退款冲回邀请佣金。
     *
     * @param order       分销订单
     * @param wasSettled  是否已结算
     */
    private void refundInviteCommission(DistributionOrder order, boolean wasSettled) {
        BigDecimal remain = inviteRemain(order);
        if (remain.compareTo(BigDecimal.ZERO) <= 0 || StringUtils.isEmpty(order.getInviteDistributionId())) {
            return;
        }
        String statusTag = wasSettled ? "SETTLED" : "FROZEN";
        walletAccountService.refundCommission(order.getInviteDistributionId(), remain, order.getOrderItemSn(),
                "order:refund:" + order.getOrderItemSn() + ":INVITE:" + statusTag, "退款冲回邀请奖励");
        order.setInviteSellBackRebate(defaultDouble(order.getInviteCommission()));
    }

    /**
     * 取消未结算订单的冻结佣金并标记为退款。
     *
     * @param order 分销订单
     */
    private void cancelFrozenCommission(DistributionOrder order) {
        refundDirectCommission(order, false);
        refundInviteCommission(order, false);
        order.setDistributionOrderStatus(DistributionOrderStatusEnum.REFUND.name());
        distributionOrderService.updateById(order);
    }

    /**
     * 计算直推佣金剩余可处理金额（扣除已冲回部分）。
     *
     * @param order 分销订单
     * @return 剩余直推佣金
     */
    private BigDecimal directRemain(DistributionOrder order) {
        return BigDecimal.valueOf(defaultDouble(order.getDirectCommission()))
                .subtract(BigDecimal.valueOf(defaultDouble(order.getSellBackRebate())));
    }

    /**
     * 计算邀请佣金剩余可处理金额（扣除已冲回部分）。
     *
     * @param order 分销订单
     * @return 剩余邀请佣金
     */
    private BigDecimal inviteRemain(DistributionOrder order) {
        return BigDecimal.valueOf(defaultDouble(order.getInviteCommission()))
                .subtract(BigDecimal.valueOf(defaultDouble(order.getInviteSellBackRebate())));
    }

    /**
     * 生成佣金冻结幂等键。
     *
     * @param orderItemSn     订单明细编号
     * @param type            佣金类型（DIRECT/INVITE）
     * @param distributionId  分销员 ID
     * @return 冻结幂等键
     */
    private String freezeKey(String orderItemSn, String type, String distributionId) {
        return "reward:freeze:" + orderItemSn + ":" + type + ":" + distributionId;
    }

    /**
     * 生成佣金结算幂等键。
     *
     * @param orderItemSn     订单明细编号
     * @param type            佣金类型（DIRECT/INVITE）
     * @param distributionId  分销员 ID
     * @return 结算幂等键
     */
    private String settleKey(String orderItemSn, String type, String distributionId) {
        return "reward:settle:" + orderItemSn + ":" + type + ":" + distributionId;
    }

    /**
     * 将 Double 转为 double，null 时返回 0。
     *
     * @param value 原始值
     * @return 非空 double 值
     */
    private double defaultDouble(Double value) {
        return value == null ? 0D : value;
    }

    /**
     * 解析订单的直推/邀请受益人及等级信息。
     *
     * @param order              主订单
     * @param settlementSetting  分销结算配置
     * @param buyerDistribution   买家对应的分销员（买家是分销员时非空）
     * @param buyerIsDistributor  买家是否已通过分销员（仅分佣分支，不是行上自购标志）
     * @param distributionCache   分销员缓存
     * @param gradeCache          等级缓存
     * @return 佣金分配上下文
     */
    private CommissionContext resolveCommissionContext(Order order,
                                                       DistributionSettlementSetting settlementSetting,
                                                       Distribution buyerDistribution,
                                                       boolean buyerIsDistributor,
                                                       Map<String, Distribution> distributionCache,
                                                       Map<String, DistributionGrade> gradeCache) {
        CommissionContext context = new CommissionContext();
        String mode = settlementSetting.getSelfPurchaseCommissionMode() == null
                ? DistributionSelfPurchaseModeEnum.SELF.name()
                : settlementSetting.getSelfPurchaseCommissionMode();

        DistributionCustomerRelation relation = customerRelationService.getActiveRelation(order.getMemberId());
        String directBeneficiaryId = null;
        // 买家是分销员：按配置决定佣金归买家本人还是绑定分销员
        if (buyerIsDistributor) {
            if (DistributionSelfPurchaseModeEnum.SELF.name().equals(mode)) {
                directBeneficiaryId = buyerDistribution.getId();
            } else if (DistributionSelfPurchaseModeEnum.BOUND_DISTRIBUTOR.name().equals(mode)) {
                directBeneficiaryId = relation == null ? null : relation.getDistributionId();
            }
        } else {
            // 推广单：优先取客户绑定关系，其次取订单快照中的分销员
            if (relation != null && relation.getDistributionId() != null) {
                directBeneficiaryId = relation.getDistributionId();
            } else if (order.getDistributionId() != null) {
                directBeneficiaryId = order.getDistributionId();
            }
        }

        if (directBeneficiaryId != null) {
            Distribution directDistribution = getCachedDistribution(directBeneficiaryId, distributionCache);
            // 仅审核通过的分销员可作为直推受益人
            if (directDistribution != null
                    && DistributionStatusEnum.PASS.name().equals(directDistribution.getDistributionStatus())) {
                context.directDistribution = directDistribution;
                context.directGrade = getCachedGrade(directDistribution.getGradeId(), gradeCache);
                context.performanceDistributionId = directBeneficiaryId;
            }
        }

        if (directBeneficiaryId != null && context.directDistribution != null) {
            // 邀请奖励归直推受益人的上级分销员
            String parentDistributionId = context.directDistribution.getParentDistributionId();
            if (StringUtils.isNotEmpty(parentDistributionId)) {
                Distribution parentDistribution = getCachedDistribution(parentDistributionId, distributionCache);
                if (parentDistribution != null
                        && DistributionStatusEnum.PASS.name().equals(parentDistribution.getDistributionStatus())) {
                    context.inviteDistribution = parentDistribution;
                    context.inviteGrade = getCachedGrade(parentDistribution.getGradeId(), gradeCache);
                }
            }
        }
        if (context.performanceDistributionId == null && context.inviteDistribution != null) {
            // 无直推受益人时，业绩仍归属邀请链上的分销员
            context.performanceDistributionId = directBeneficiaryId;
        }
        return context;
    }

    /**
     * 批量加载订单明细对应的分销商品，按 SKU ID 索引。
     *
     * @param orderItems 订单明细列表
     * @return SKU ID 到分销商品的映射
     */
    private Map<String, DistributionGoods> loadDistributionGoodsMap(List<OrderItem> orderItems) {
        List<String> skuIds = orderItems.stream()
                .map(OrderItem::getSkuId)
                .filter(skuId -> skuId != null && !skuId.isBlank())
                .distinct()
                .toList();
        if (skuIds.isEmpty()) {
            return Map.of();
        }
        return queryInBatches(skuIds, batch -> distributionGoodsService.distributionGoods(batch))
                .stream()
                .filter(goods -> goods.getSkuId() != null)
                .collect(Collectors.toMap(DistributionGoods::getSkuId, goods -> goods, (left, right) -> left));
    }

    /**
     * 加载订单下已存在的分销订单明细编号集合。
     *
     * @param orderSn 主订单编号
     * @return 已写入的分销订单明细编号
     */
    private Set<String> loadExistingDistributionOrderItemSns(String orderSn) {
        return distributionOrderService.list(new LambdaQueryWrapper<DistributionOrder>()
                        .eq(DistributionOrder::getOrderSn, orderSn)
                        .select(DistributionOrder::getOrderItemSn))
                .stream()
                .map(DistributionOrder::getOrderItemSn)
                .filter(sn -> sn != null && !sn.isBlank())
                .collect(Collectors.toSet());
    }

    /**
     * 加载订单下全部分销订单，按订单明细编号索引。
     *
     * @param orderSn 主订单编号
     * @return 订单明细编号到分销订单的映射
     */
    private Map<String, DistributionOrder> loadDistributionOrdersByOrderSn(String orderSn) {
        return distributionOrderService.list(new LambdaQueryWrapper<DistributionOrder>()
                        .eq(DistributionOrder::getOrderSn, orderSn))
                .stream()
                .filter(order -> order.getOrderItemSn() != null && !order.getOrderItemSn().isBlank())
                .collect(Collectors.toMap(DistributionOrder::getOrderItemSn, order -> order, (left, right) -> left));
    }

    /**
     * 分批执行 IN 查询并合并结果，避免单次查询 ID 过多。
     *
     * @param ids   ID 列表
     * @param query 单批查询函数
     * @param <T>   结果类型
     * @return 合并后的查询结果
     */
    private <T> List<T> queryInBatches(List<String> ids, Function<List<String>, List<T>> query) {
        if (ids.isEmpty()) {
            return List.of();
        }
        List<T> result = new ArrayList<>();
        for (int i = 0; i < ids.size(); i += IN_QUERY_BATCH_SIZE) {
            List<String> batch = ids.subList(i, Math.min(i + IN_QUERY_BATCH_SIZE, ids.size()));
            result.addAll(query.apply(batch));
        }
        return result;
    }

    /**
     * 从缓存获取分销员，未命中时查库并写入缓存。
     *
     * @param distributionId 分销员 ID
     * @param cache          分销员缓存
     * @return 分销员，ID 无效时返回 null
     */
    private Distribution getCachedDistribution(String distributionId, Map<String, Distribution> cache) {
        if (distributionId == null || distributionId.isBlank()) {
            return null;
        }
        return cache.computeIfAbsent(distributionId, distributionService::getById);
    }

    /**
     * 从缓存获取分销等级，未命中时解析并写入缓存。
     *
     * @param gradeId 等级 ID
     * @param cache   等级缓存
     * @return 分销等级（含默认回退）
     */
    private DistributionGrade getCachedGrade(String gradeId, Map<String, DistributionGrade> cache) {
        String cacheKey = gradeId == null || gradeId.isBlank() ? "__default__" : gradeId;
        return cache.computeIfAbsent(cacheKey, key -> distributionGradeService.resolveGradeWithFallback(gradeId));
    }

    /**
     * 确保分销订单存在：优先读缓存/库，不存在则新建并持久化。
     *
     * @param order                          主订单
     * @param orderItem                      订单明细
     * @param ctx                            订单级佣金上下文
     * @param payload                        明细写入载荷
     * @param distributionCache              分销员缓存
     * @param existingDistributionOrderItems 已存在的明细编号集合
     * @param distributionOrderByItem        内存中的分销订单映射
     * @return 分销订单，无直推受益人时返回 null
     */
    private DistributionOrder ensureDistributionOrder(Order order,
                                                      OrderItem orderItem,
                                                      CommissionContext ctx,
                                                      WritePayload payload,
                                                      Map<String, Distribution> distributionCache,
                                                      Set<String> existingDistributionOrderItems,
                                                      Map<String, DistributionOrder> distributionOrderByItem) {
        if (payload == null || payload.distributionId == null) {
            return null;
        }
        String distributionId = payload.distributionId;
        String orderItemSn = orderItem.getSn();
        DistributionOrder existing = distributionOrderByItem.get(orderItemSn);
        if (existing != null) {
            return existing;
        }
        // 内存未命中但库中已存在（如并发写入），回查后跳过新建
        if (existingDistributionOrderItems.contains(orderItemSn)) {
            return getDistributionOrderByItemSn(orderItemSn);
        }

        Distribution distribution = getCachedDistribution(distributionId, distributionCache);
        boolean rowSelfPurchase = ctx != null
                && ctx.buyerDistributionId != null
                && ctx.buyerDistributionId.equals(distributionId);
        Date paymentTime = order.getPaymentTime() != null ? order.getPaymentTime() : order.getCreateTime();
        DistributionOrder distributionOrder = new DistributionOrder();
        distributionOrder.setDistributionOrderStatus(DistributionOrderStatusEnum.NO_COMPLETED.name());
        distributionOrder.setMemberId(order.getMemberId());
        distributionOrder.setMemberName(order.getMemberName());
        distributionOrder.setDistributionId(distributionId);
        distributionOrder.setDistributionName(distribution == null ? null : distribution.getMemberName());
        distributionOrder.setInviteDistributionId(payload.inviteDistributionId);
        distributionOrder.setInviteDistributionName(payload.inviteDistributionName);
        distributionOrder.setPerformanceDistributorId(distributionId);
        distributionOrder.setSelfPurchase(rowSelfPurchase);
        distributionOrder.setPaymentTime(paymentTime);
        distributionOrder.setCommissionPool(payload.commissionPool);
        distributionOrder.setDirectCommission(
                payload.directCommission == null ? 0D : payload.directCommission.doubleValue());
        distributionOrder.setInviteCommission(
                payload.inviteCommission == null || payload.inviteCommission.compareTo(BigDecimal.ZERO) <= 0
                        ? null : payload.inviteCommission.doubleValue());
        distributionOrder.setSellBackRebate(0D);
        distributionOrder.setInviteSellBackRebate(0D);
        distributionOrder.setStoreId(order.getStoreId());
        distributionOrder.setStoreName(order.getStoreName());
        distributionOrder.setOrderSn(order.getSn());
        distributionOrder.setOrderItemSn(orderItemSn);
        distributionOrder.setGoodsId(orderItem.getGoodsId());
        distributionOrder.setGoodsName(orderItem.getGoodsName());
        distributionOrder.setSkuId(orderItem.getSkuId());
        distributionOrder.setSpecs(orderItem.getSpecs());
        distributionOrder.setImage(orderItem.getImage());
        distributionOrder.setNum(orderItem.getNum());
        distributionOrder.setRefundNum(0);
        try {
            distributionOrderService.save(distributionOrder);
            existingDistributionOrderItems.add(orderItemSn);
            distributionOrderByItem.put(orderItemSn, distributionOrder);
            return distributionOrder;
        } catch (DuplicateKeyException ex) {
            // uk_do_order_item_sn：afterCommit 与 MQ 补偿并发时以已存在记录为准
            log.warn("分销订单已存在，跳过重复写入 orderItemSn={}", orderItemSn);
            existingDistributionOrderItems.add(orderItemSn);
            return getDistributionOrderByItemSn(orderItemSn);
        }
    }

    /**
     * 累计分销员有效业绩销售额增量。
     *
     * @param salesDelta      销售额增量汇总
     * @param distributionIds 涉及的分销员 ID 集合
     * @param distributionId  业绩归属分销员 ID
     * @param orderItem       订单明细
     */
    private void accumulatePerformanceSales(Map<String, BigDecimal> salesDelta,
                                            Set<String> distributionIds,
                                            String distributionId,
                                            OrderItem orderItem) {
        if (distributionId == null) {
            return;
        }
        BigDecimal itemSales = BigDecimal.valueOf(orderItem.getFlowPrice() == null ? 0D : orderItem.getFlowPrice());
        salesDelta.merge(distributionId, itemSales, BigDecimal::add);
        distributionIds.add(distributionId);
    }

    /**
     * 将累计的有效业绩销售额写入分销员并刷新等级。
     *
     * @param salesDelta      销售额增量汇总
     * @param distributionIds 待刷新等级的分销员 ID 集合
     */
    private void flushPerformanceSales(Map<String, BigDecimal> salesDelta, Set<String> distributionIds) {
        if (salesDelta.isEmpty()) {
            return;
        }
        for (Map.Entry<String, BigDecimal> entry : salesDelta.entrySet()) {
            Distribution distribution = distributionService.getById(entry.getKey());
            if (distribution == null) {
                continue;
            }
            BigDecimal sales = defaultAmount(distribution.getValidSalesAmount()).add(entry.getValue());
            distribution.setValidSalesAmount(sales);
            distributionService.updateById(distribution);
        }
        distributionIds.forEach(distributionGradeService::refreshGrade);
    }

    /**
     * 累加分销员有效佣金指标。
     *
     * @param distributionId 分销员 ID
     * @param amount         本次结算佣金
     */
    private void addCommissionMetrics(String distributionId, BigDecimal amount) {
        Distribution distribution = distributionService.getById(distributionId);
        if (distribution == null) {
            return;
        }
        BigDecimal commission = defaultAmount(distribution.getValidCommissionAmount()).add(amount);
        distribution.setValidCommissionAmount(commission);
        distributionService.updateById(distribution);
    }

    /**
     * 读取分销结算配置，缺失时使用自购默认模式。
     *
     * @return 分销结算配置
     */
    private DistributionSettlementSetting getSettlementSetting() {
        Setting setting = settingService.get(SettingEnum.DISTRIBUTION_SETTLEMENT_SETTING.name());
        if (setting == null || setting.getSettingValue() == null) {
            DistributionSettlementSetting settlementSetting = new DistributionSettlementSetting();
            settlementSetting.setSelfPurchaseCommissionMode(DistributionSelfPurchaseModeEnum.SELF.name());
            return settlementSetting;
        }
        return JSONUtil.toBean(setting.getSettingValue(), DistributionSettlementSetting.class);
    }

    /**
     * 将 BigDecimal 空值转为 ZERO。
     *
     * @param value 原始金额
     * @return 非空金额
     */
    private BigDecimal defaultAmount(BigDecimal value) {
        return value == null ? BigDecimal.ZERO : value;
    }

    /**
     * 佣金分配上下文，缓存直推/邀请受益人及业绩归属分销员。
     * 行上自购用 buyerDistributionId 与本行 distributionId 比较，不要和分佣分支的 buyerIsDistributor 混用。
     */
    private static final class CommissionContext {
        private String buyerDistributionId;
        private Distribution directDistribution;
        private DistributionGrade directGrade;
        private Distribution inviteDistribution;
        private DistributionGrade inviteGrade;
        private String performanceDistributionId;

        /**
         * 是否存在有效的直推奖励受益人。
         *
         * @return 直推分销员及等级均已解析时为 true
         */
        private boolean hasDirectReward() {
            return directDistribution != null && directGrade != null;
        }

        /**
         * 是否存在有效的邀请奖励受益人。
         *
         * @return 邀请分销员及等级均已解析时为 true
         */
        private boolean hasInviteReward() {
            return inviteDistribution != null && inviteGrade != null;
        }
    }

    /**
     * 单条明细写入载荷，避免 ensureDistributionOrder 参数膨胀。
     */
    private static final class WritePayload {
        private String distributionId;
        private String inviteDistributionId;
        private String inviteDistributionName;
        private BigDecimal directCommission;
        private BigDecimal inviteCommission;
        private BigDecimal commissionPool;
    }
}
