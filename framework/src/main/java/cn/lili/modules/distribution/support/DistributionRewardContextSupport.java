package cn.lili.modules.distribution.support;

import cn.hutool.json.JSONUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionCustomerRelation;
import cn.lili.modules.distribution.entity.dos.DistributionGoods;
import cn.lili.modules.distribution.entity.dos.DistributionGrade;
import cn.lili.modules.distribution.entity.enums.DistributionSelfPurchaseModeEnum;
import cn.lili.modules.distribution.entity.enums.DistributionStatusEnum;
import cn.lili.modules.distribution.service.DistributionCustomerRelationService;
import cn.lili.modules.distribution.service.DistributionGoodsService;
import cn.lili.modules.distribution.service.DistributionGradeService;
import cn.lili.modules.distribution.service.DistributionService;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.DistributionSettlementSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * 分销佣金上下文解析与分销员业绩/佣金指标刷新支持类。
 * 负责读取分销结算配置、解析直推/邀请受益人及等级、批量加载分销商品，以及更新有效销售额与有效佣金。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
@Component
public class DistributionRewardContextSupport {

    private static final int IN_QUERY_BATCH_SIZE = 500;

    private final DistributionGoodsService distributionGoodsService;

    private final DistributionService distributionService;

    private final DistributionGradeService distributionGradeService;

    private final DistributionCustomerRelationService customerRelationService;

    private final SettingService settingService;

    /**
     * 注入佣金上下文解析及指标更新所需的必需服务。
     *
     * @param distributionGoodsService 分销商品服务
     * @param distributionService 分销员服务
     * @param distributionGradeService 分销等级服务
     * @param customerRelationService 客户绑定关系服务
     * @param settingService 系统配置服务
     */
    public DistributionRewardContextSupport(DistributionGoodsService distributionGoodsService,
                                            DistributionService distributionService,
                                            DistributionGradeService distributionGradeService,
                                            DistributionCustomerRelationService customerRelationService,
                                            SettingService settingService) {
        this.distributionGoodsService = distributionGoodsService;
        this.distributionService = distributionService;
        this.distributionGradeService = distributionGradeService;
        this.customerRelationService = customerRelationService;
        this.settingService = settingService;
    }

    /**
     * 解析订单的直推/邀请受益人及等级信息。
     *
     * @param order              主订单
     * @param settlementSetting  分销结算配置
     * @param buyerDistribution  买家对应的分销员（买家是分销员时非空）
     * @param buyerIsDistributor 买家是否已通过分销员审核
     * @param distributionCache  分销员缓存
     * @param gradeCache         等级缓存
     * @return 佣金分配上下文
     */
    public CommissionContext resolveCommissionContext(Order order,
                                                      DistributionSettlementSetting settlementSetting,
                                                      Distribution buyerDistribution,
                                                      boolean buyerIsDistributor,
                                                      Map<String, Distribution> distributionCache,
                                                      Map<String, DistributionGrade> gradeCache) {
        CommissionContext context = new CommissionContext();
        context.buyerDistributionId = buyerDistribution == null ? null : buyerDistribution.getId();
        String mode = settlementSetting.getSelfPurchaseCommissionMode() == null
                ? DistributionSelfPurchaseModeEnum.SELF.name()
                : settlementSetting.getSelfPurchaseCommissionMode();

        DistributionCustomerRelation relation = customerRelationService.getActiveRelation(order.getMemberId());
        String directBeneficiaryId = null;
        if (buyerIsDistributor) {
            if (DistributionSelfPurchaseModeEnum.SELF.name().equals(mode)) {
                directBeneficiaryId = buyerDistribution.getId();
            } else if (DistributionSelfPurchaseModeEnum.BOUND_DISTRIBUTOR.name().equals(mode)) {
                directBeneficiaryId = relation == null ? null : relation.getDistributionId();
            }
        } else {
            if (relation != null && relation.getDistributionId() != null) {
                directBeneficiaryId = relation.getDistributionId();
            } else if (order.getDistributionId() != null) {
                directBeneficiaryId = order.getDistributionId();
            }
        }

        if (directBeneficiaryId != null) {
            Distribution directDistribution = getCachedDistribution(directBeneficiaryId, distributionCache);
            if (directDistribution != null
                    && DistributionStatusEnum.PASS.name().equals(directDistribution.getDistributionStatus())) {
                context.directDistribution = directDistribution;
                context.directGrade = getCachedGrade(directDistribution.getGradeId(), gradeCache);
                context.performanceDistributionId = directBeneficiaryId;
            }
        }

        if (directBeneficiaryId != null && context.directDistribution != null) {
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
    public Map<String, DistributionGoods> loadDistributionGoodsMap(List<OrderItem> orderItems) {
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
     * 从缓存获取分销员，未命中时查库并写入缓存。
     *
     * @param distributionId 分销员 ID
     * @param cache          分销员缓存
     * @return 分销员实体
     */
    public Distribution getCachedDistribution(String distributionId, Map<String, Distribution> cache) {
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
     * @return 分销等级
     */
    public DistributionGrade getCachedGrade(String gradeId, Map<String, DistributionGrade> cache) {
        String cacheKey = gradeId == null || gradeId.isBlank() ? "__default__" : gradeId;
        return cache.computeIfAbsent(cacheKey, key -> distributionGradeService.resolveGradeWithFallback(gradeId));
    }

    /**
     * 累计分销员有效业绩销售额增量。
     *
     * @param salesDelta      销售额增量汇总
     * @param distributionIds 涉及的分销员 ID 集合
     * @param distributionId  业绩归属分销员 ID
     * @param orderItem       订单明细
     */
    public void accumulatePerformanceSales(Map<String, BigDecimal> salesDelta,
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
    public void flushPerformanceSales(Map<String, BigDecimal> salesDelta, Set<String> distributionIds) {
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
    public void addCommissionMetrics(String distributionId, BigDecimal amount) {
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
    public DistributionSettlementSetting getSettlementSetting() {
        Setting setting = settingService.get(SettingEnum.DISTRIBUTION_SETTLEMENT_SETTING.name());
        if (setting == null || setting.getSettingValue() == null) {
            DistributionSettlementSetting settlementSetting = new DistributionSettlementSetting();
            settlementSetting.setSelfPurchaseCommissionMode(DistributionSelfPurchaseModeEnum.SELF.name());
            return settlementSetting;
        }
        return JSONUtil.toBean(setting.getSettingValue(), DistributionSettlementSetting.class);
    }

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

    private BigDecimal defaultAmount(BigDecimal value) {
        return value == null ? BigDecimal.ZERO : value;
    }

    /**
     * 佣金分配上下文，缓存直推/邀请受益人及业绩归属分销员。
     * 行上自购用 buyerDistributionId 与本行 distributionId 比较，不与分佣分支的 buyerIsDistributor 混用。
     *
     * @author Bulbasaur
     * @since 2026-11-13
     */
    public static final class CommissionContext {
        public String buyerDistributionId;
        public Distribution directDistribution;
        public DistributionGrade directGrade;
        public Distribution inviteDistribution;
        public DistributionGrade inviteGrade;
        public String performanceDistributionId;

        /**
         * 是否存在有效的直推奖励受益人。
         *
         * @return 直推分销员及等级均已解析时为 true
         */
        public boolean hasDirectReward() {
            return directDistribution != null && directGrade != null;
        }

        /**
         * 是否存在有效的邀请奖励受益人。
         *
         * @return 邀请分销员及等级均已解析时为 true
         */
        public boolean hasInviteReward() {
            return inviteDistribution != null && inviteGrade != null;
        }
    }
}
