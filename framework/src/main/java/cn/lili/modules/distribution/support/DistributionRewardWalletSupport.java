package cn.lili.modules.distribution.support;

import cn.lili.common.utils.StringUtils;
import cn.lili.modules.distribution.entity.dos.DistributionOrder;
import cn.lili.modules.distribution.service.DistributionWalletAccountService;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.Map;
import java.util.Set;

/**
 * 分销佣金钱包账务支持类，统一处理直推与邀请佣金的冻结、解冻结算、
 * 退款冲回及剩余可处理佣金核算，保证账务幂等键一致。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
@Component
public class DistributionRewardWalletSupport {

    private final DistributionWalletAccountService walletAccountService;

    /**
     * 注入必需的钱包账务服务，沿用调用方事务及钱包幂等规则。
     *
     * @param walletAccountService 分销钱包账务服务
     */
    public DistributionRewardWalletSupport(DistributionWalletAccountService walletAccountService) {
        this.walletAccountService = walletAccountService;
    }

    /**
     * 冻结分销订单中的直推与邀请佣金。
     *
     * @param order 分销订单
     */
    public void freezeCommissions(DistributionOrder order) {
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
     * 结算直推与邀请佣金并累计待刷新等级的分销员ID。
     *
     * @param order           分销订单
     * @param commissionDelta 分销员佣金增量汇总
     * @param gradeRefreshIds 待刷新等级的分销员ID集合
     */
    public void settleCommissions(DistributionOrder order,
                                  Map<String, BigDecimal> commissionDelta,
                                  Set<String> gradeRefreshIds) {
        settleSingleCommission(order.getDistributionId(), directRemain(order), order.getOrderItemSn(),
                "DIRECT", "佣金解冻结算", commissionDelta, gradeRefreshIds);
        settleSingleCommission(order.getInviteDistributionId(), inviteRemain(order), order.getOrderItemSn(),
                "INVITE", "邀请奖励解冻结算", commissionDelta, gradeRefreshIds);
    }

    /**
     * 退款冲回直推与邀请佣金，并回填订单上的退回金额字段。
     *
     * @param order      分销订单
     * @param wasSettled 是否已结算（已结算走 SETTLED 冲回，未结算走 FROZEN 取消）
     */
    public void refundCommissions(DistributionOrder order, boolean wasSettled) {
        String statusTag = wasSettled ? "SETTLED" : "FROZEN";
        BigDecimal directRemain = directRemain(order);
        if (directRemain.compareTo(BigDecimal.ZERO) > 0 && StringUtils.isNotEmpty(order.getDistributionId())) {
            walletAccountService.refundCommission(order.getDistributionId(), directRemain, order.getOrderItemSn(),
                    "order:refund:" + order.getOrderItemSn() + ":DIRECT:" + statusTag, "退款冲回佣金");
            order.setSellBackRebate(defaultDouble(order.getDirectCommission()));
        }
        BigDecimal inviteRemain = inviteRemain(order);
        if (inviteRemain.compareTo(BigDecimal.ZERO) > 0 && StringUtils.isNotEmpty(order.getInviteDistributionId())) {
            walletAccountService.refundCommission(order.getInviteDistributionId(), inviteRemain, order.getOrderItemSn(),
                    "order:refund:" + order.getOrderItemSn() + ":INVITE:" + statusTag, "退款冲回邀请奖励");
            order.setInviteSellBackRebate(defaultDouble(order.getInviteCommission()));
        }
    }

    private void settleSingleCommission(String distributionId,
                                        BigDecimal amount,
                                        String orderItemSn,
                                        String type,
                                        String remark,
                                        Map<String, BigDecimal> commissionDelta,
                                        Set<String> gradeRefreshIds) {
        if (amount.compareTo(BigDecimal.ZERO) <= 0 || StringUtils.isEmpty(distributionId)) {
            return;
        }
        walletAccountService.settleCommission(distributionId, amount, orderItemSn,
                settleKey(orderItemSn, type, distributionId), remark);
        commissionDelta.merge(distributionId, amount, BigDecimal::add);
        gradeRefreshIds.add(distributionId);
    }

    /**
     * 计算直推佣金剩余可处理金额（扣除已冲回部分）。
     *
     * @param order 分销订单
     * @return 剩余直推佣金
     */
    public BigDecimal directRemain(DistributionOrder order) {
        return BigDecimal.valueOf(defaultDouble(order.getDirectCommission()))
                .subtract(BigDecimal.valueOf(defaultDouble(order.getSellBackRebate())));
    }

    /**
     * 计算邀请佣金剩余可处理金额（扣除已冲回部分）。
     *
     * @param order 分销订单
     * @return 剩余邀请佣金
     */
    public BigDecimal inviteRemain(DistributionOrder order) {
        return BigDecimal.valueOf(defaultDouble(order.getInviteCommission()))
                .subtract(BigDecimal.valueOf(defaultDouble(order.getInviteSellBackRebate())));
    }

    private String freezeKey(String orderItemSn, String type, String distributionId) {
        return "reward:freeze:" + orderItemSn + ":" + type + ":" + distributionId;
    }

    private String settleKey(String orderItemSn, String type, String distributionId) {
        return "reward:settle:" + orderItemSn + ":" + type + ":" + distributionId;
    }

    private double defaultDouble(Double value) {
        return value == null ? 0D : value;
    }
}
