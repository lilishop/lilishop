package cn.lili.modules.distribution.service;

import java.math.BigDecimal;

/**
 * 分销钱包账户服务。
 * <p>
 * 管理分销员佣金冻结、结算、退款冲回及提现相关资金变动，
 * 所有操作均支持幂等键防重复。
 * </p>
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
public interface DistributionWalletAccountService {

    /**
     * 冻结佣金（订单支付奖励生成时）。
     *
     * @param distributionId 分销员 ID
     * @param amount         冻结金额
     * @param bizSn          业务单号
     * @param idempotentKey  幂等键
     * @param remark         备注
     */
    void freezeCommission(String distributionId, BigDecimal amount, String bizSn, String idempotentKey, String remark);

    /**
     * 结算佣金（子订单完成时，冻结转可提现）。
     *
     * @param distributionId 分销员 ID
     * @param amount         结算金额
     * @param bizSn          业务单号
     * @param idempotentKey  幂等键
     * @param remark         备注
     */
    void settleCommission(String distributionId, BigDecimal amount, String bizSn, String idempotentKey, String remark);

    /**
     * 退款冲回佣金。
     *
     * @param distributionId 分销员 ID
     * @param amount         冲回金额
     * @param bizSn          业务单号
     * @param idempotentKey  幂等键
     * @param remark         备注
     */
    void refundCommission(String distributionId, BigDecimal amount, String bizSn, String idempotentKey, String remark);

    /**
     * 申请提现（可提现金额转提现冻结）。
     *
     * @param distributionId 分销员 ID
     * @param amount         提现金额
     * @param bizSn          业务单号
     * @param idempotentKey  幂等键
     * @param remark         备注
     */
    void applyWithdraw(String distributionId, BigDecimal amount, String bizSn, String idempotentKey, String remark);

    /**
     * 提现审核通过。
     *
     * @param distributionId 分销员 ID
     * @param amount         提现金额
     * @param bizSn          业务单号
     * @param idempotentKey  幂等键
     * @param remark         备注
     */
    void approveWithdraw(String distributionId, BigDecimal amount, String bizSn, String idempotentKey, String remark);

    /**
     * 提现审核拒绝（解冻回可提现）。
     *
     * @param distributionId 分销员 ID
     * @param amount         提现金额
     * @param bizSn          业务单号
     * @param idempotentKey  幂等键
     * @param remark         备注
     */
    void rejectWithdraw(String distributionId, BigDecimal amount, String bizSn, String idempotentKey, String remark);
}
