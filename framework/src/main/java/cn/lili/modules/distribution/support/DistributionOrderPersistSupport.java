package cn.lili.modules.distribution.support;

import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionOrder;
import cn.lili.modules.distribution.entity.enums.DistributionOrderStatusEnum;
import cn.lili.modules.distribution.mapper.DistributionOrderMapper;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.service.OrderService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import lombok.extern.slf4j.Slf4j;
import io.swagger.v3.oas.annotations.media.Schema;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * 分销订单持久化支持类，通过订单服务查询主订单，统一使用Mapper操作分销订单。
 * 不依赖分销订单服务，避免其回调奖励服务形成循环依赖；写入参与调用方奖励事务。
 * 负责分销订单的幂等写入（基于 orderItemSn 唯一键防并发重入）、批量查询加载以及状态更新。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
@Slf4j
@Component
public class DistributionOrderPersistSupport {

    private final OrderService orderService;
    private final DistributionOrderMapper distributionOrderMapper;

    /**
     * 注入必需的主订单查询服务和分销订单存储，缺少依赖时由Spring阻止启动。
     *
     * @param orderService 主订单查询服务
     * @param distributionOrderMapper 分销订单Mapper
     */
    public DistributionOrderPersistSupport(OrderService orderService,
                                           DistributionOrderMapper distributionOrderMapper) {
        this.orderService = orderService;
        this.distributionOrderMapper = distributionOrderMapper;
    }

    /**
     * 按订单编号查询主订单。
     *
     * @param orderSn 订单编号
     * @return 订单实体
     */
    public Order getOrderBySn(String orderSn) {
        return orderService.getBySn(orderSn);
    }

    /**
     * 按订单明细编号查询分销订单。
     *
     * @param orderItemSn 订单明细编号
     * @return 分销订单，不存在或参数为空时返回 null
     */
    public DistributionOrder getDistributionOrderByItemSn(String orderItemSn) {
        if (orderItemSn == null || orderItemSn.isBlank()) {
            return null;
        }
        return getOneDistributionOrder(new LambdaQueryWrapper<DistributionOrder>()
                .eq(DistributionOrder::getOrderItemSn, orderItemSn)
                .last("LIMIT 1"));
    }

    /**
     * 加载订单下已存在的分销订单明细编号集合。
     *
     * @param orderSn 主订单编号
     * @return 已写入的分销订单明细编号集合
     */
    public Set<String> loadExistingDistributionOrderItemSns(String orderSn) {
        return listDistributionOrders(new LambdaQueryWrapper<DistributionOrder>()
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
    public Map<String, DistributionOrder> loadDistributionOrdersByOrderSn(String orderSn) {
        return listDistributionOrders(new LambdaQueryWrapper<DistributionOrder>()
                        .eq(DistributionOrder::getOrderSn, orderSn))
                .stream()
                .filter(order -> order.getOrderItemSn() != null && !order.getOrderItemSn().isBlank())
                .collect(Collectors.toMap(DistributionOrder::getOrderItemSn, order -> order, (left, right) -> left));
    }

    /**
     * 确保分销订单存在：优先读缓存/库，不存在则新建并幂等持久化。
     *
     * @param order                          主订单
     * @param orderItem                      订单明细
     * @param ctx                            订单级佣金上下文
     * @param payload                        明细写入载荷
     * @param contextSupport                 上下文支持类（用于读缓存分销员）
     * @param distributionCache              分销员缓存
     * @param existingDistributionOrderItems 已存在的明细编号集合
     * @param distributionOrderByItem        内存中的分销订单映射
     * @return 分销订单，无直推受益人时返回 null
     */
    public DistributionOrder ensureDistributionOrder(Order order,
                                                     OrderItem orderItem,
                                                     DistributionRewardContextSupport.CommissionContext ctx,
                                                     WritePayload payload,
                                                     DistributionRewardContextSupport contextSupport,
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
        if (existingDistributionOrderItems.contains(orderItemSn)) {
            return getDistributionOrderByItemSn(orderItemSn);
        }

        Distribution distribution = contextSupport.getCachedDistribution(distributionId, distributionCache);
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
            saveDistributionOrder(distributionOrder);
            existingDistributionOrderItems.add(orderItemSn);
            distributionOrderByItem.put(orderItemSn, distributionOrder);
            return distributionOrder;
        } catch (DuplicateKeyException ex) {
            log.warn("分销订单已存在，跳过重复写入 orderItemSn={}", orderItemSn);
            existingDistributionOrderItems.add(orderItemSn);
            return getDistributionOrderByItemSn(orderItemSn);
        }
    }

    /**
     * 插入分销订单，唯一键冲突向上传递，由幂等创建入口读取已有记录。
     *
     * @param order 待保存分销订单
     */
    public void saveDistributionOrder(DistributionOrder order) {
        distributionOrderMapper.insert(order);
    }

    /**
     * 在调用方事务内按主键更新分销订单的结算或退款状态。
     *
     * @param order 待更新分销订单
     */
    public void updateDistributionOrderById(DistributionOrder order) {
        distributionOrderMapper.updateById(order);
    }

    /**
     * 按服务端构造的条件查询分销订单。
     *
     * @param wrapper 查询条件
     * @return 分销订单列表
     */
    public List<DistributionOrder> listDistributionOrders(LambdaQueryWrapper<DistributionOrder> wrapper) {
        return distributionOrderMapper.selectList(wrapper);
    }

    /**
     * 按条件统计分销订单，不额外过滤业务状态。
     *
     * @param wrapper 查询条件
     * @return 匹配的记录数量
     */
    public long countDistributionOrders(LambdaQueryWrapper<DistributionOrder> wrapper) {
        return distributionOrderMapper.selectCount(wrapper);
    }

    /**
     * 按条件读取单条分销订单，调用方按订单明细编号及LIMIT 1限制结果。
     *
     * @param wrapper 查询条件
     * @return 分销订单，不存在时为null
     */
    public DistributionOrder getOneDistributionOrder(LambdaQueryWrapper<DistributionOrder> wrapper) {
        return distributionOrderMapper.selectOne(wrapper);
    }

    /**
     * 单条明细写入载荷，避免 ensureDistributionOrder 参数膨胀。
     *
     * @author Bulbasaur
     * @since 2026-11-13
     */
    public static final class WritePayload {
        @Schema(description = "直推受益分销员ID，对应li_distribution.id")
        public String distributionId;
        @Schema(description = "邀请受益分销员ID，对应li_distribution.id，无邀请奖励时为空")
        public String inviteDistributionId;
        @Schema(description = "邀请受益分销员名称快照")
        public String inviteDistributionName;
        @Schema(description = "本订单明细直推佣金快照，单位元")
        public BigDecimal directCommission;
        @Schema(description = "本订单明细邀请佣金快照，单位元")
        public BigDecimal inviteCommission;
        @Schema(description = "商品佣金乘购买数量计算的明细佣金池快照，单位元")
        public BigDecimal commissionPool;
    }
}
