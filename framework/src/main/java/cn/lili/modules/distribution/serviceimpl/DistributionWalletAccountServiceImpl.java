package cn.lili.modules.distribution.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionWalletLog;
import cn.lili.modules.distribution.entity.enums.DistributionWalletLogTypeEnum;
import cn.lili.modules.distribution.mapper.DistributionWalletLogMapper;
import cn.lili.modules.distribution.service.DistributionService;
import cn.lili.modules.distribution.service.DistributionWalletAccountService;
import cn.lili.modules.distribution.util.DistributionCommissionCalculator;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;

/**
 * 分销钱包账户服务实现
 *
 * @author pikachu
 */
@Slf4j
@Service
public class DistributionWalletAccountServiceImpl implements DistributionWalletAccountService {

    @Autowired
    private DistributionService distributionService;

    @Autowired
    private DistributionWalletLogMapper distributionWalletLogMapper;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void freezeCommission(String distributionId, BigDecimal amount, String bizSn, String idempotentKey, String remark) {
        mutateWallet(distributionId, amount, bizSn, idempotentKey, remark,
                DistributionWalletLogTypeEnum.COMMISSION_FREEZE, WalletMutation.FREEZE_COMMISSION);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void settleCommission(String distributionId, BigDecimal amount, String bizSn, String idempotentKey, String remark) {
        mutateWallet(distributionId, amount, bizSn, idempotentKey, remark,
                DistributionWalletLogTypeEnum.COMMISSION_SETTLE, WalletMutation.SETTLE_COMMISSION);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void refundCommission(String distributionId, BigDecimal amount, String bizSn, String idempotentKey, String remark) {
        mutateWallet(distributionId, amount, bizSn, idempotentKey, remark,
                DistributionWalletLogTypeEnum.COMMISSION_REFUND, WalletMutation.REFUND_COMMISSION);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void applyWithdraw(String distributionId, BigDecimal amount, String bizSn, String idempotentKey, String remark) {
        mutateWallet(distributionId, amount, bizSn, idempotentKey, remark,
                DistributionWalletLogTypeEnum.WITHDRAW_APPLY, WalletMutation.APPLY_WITHDRAW);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void approveWithdraw(String distributionId, BigDecimal amount, String bizSn, String idempotentKey, String remark) {
        mutateWallet(distributionId, amount, bizSn, idempotentKey, remark,
                DistributionWalletLogTypeEnum.WITHDRAW_APPROVE, WalletMutation.APPROVE_WITHDRAW);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void rejectWithdraw(String distributionId, BigDecimal amount, String bizSn, String idempotentKey, String remark) {
        mutateWallet(distributionId, amount, bizSn, idempotentKey, remark,
                DistributionWalletLogTypeEnum.WITHDRAW_REJECT, WalletMutation.REJECT_WITHDRAW);
    }

    /**
     * 执行钱包余额变更并记录流水（含幂等校验）。
     *
     * @param distributionId 分销员ID
     * @param amount         变动金额
     * @param bizSn          业务单号
     * @param idempotentKey  幂等键
     * @param remark         备注
     * @param logType        流水类型
     * @param mutation       变动策略
     */
    private void mutateWallet(String distributionId, BigDecimal amount, String bizSn, String idempotentKey, String remark,
                              DistributionWalletLogTypeEnum logType, WalletMutation mutation) {
        BigDecimal changeAmount = DistributionCommissionCalculator.toMoney(amount);
        if (changeAmount.compareTo(BigDecimal.ZERO) <= 0) {
            throw new ServiceException(ResultCode.ERROR);
        }
        // 幂等键已处理则直接返回，避免重复记账
        if (existsIdempotent(idempotentKey)) {
            return;
        }
        Distribution distribution = distributionService.getById(distributionId);
        if (distribution == null) {
            throw new ServiceException(ResultCode.DISTRIBUTION_NOT_EXIST);
        }

        // 变更前快照 -> 应用策略 -> 持久化 -> 记录流水
        WalletSnapshot before = WalletSnapshot.from(distribution);
        mutation.apply(distribution, changeAmount);
        if (!distributionService.updateById(distribution)) {
            throw new ServiceException(ResultCode.ERROR);
        }
        WalletSnapshot after = WalletSnapshot.from(distribution);
        saveLog(distributionId, logType, bizSn, changeAmount, before, after, idempotentKey, remark);
    }

    /**
     * 判断幂等键是否已存在。
     *
     * @param idempotentKey 幂等键
     * @return 已存在返回 true
     */
    private boolean existsIdempotent(String idempotentKey) {
        return distributionWalletLogMapper.selectCount(new LambdaQueryWrapper<DistributionWalletLog>()
                .eq(DistributionWalletLog::getIdempotentKey, idempotentKey)) > 0;
    }

    /**
     * 保存钱包变动流水。
     *
     * @param distributionId 分销员ID
     * @param logType        流水类型
     * @param bizSn          业务单号
     * @param changeAmount   变动金额
     * @param before         变更前快照
     * @param after          变更后快照
     * @param idempotentKey  幂等键
     * @param remark         备注
     */
    private void saveLog(String distributionId, DistributionWalletLogTypeEnum logType, String bizSn, BigDecimal changeAmount,
                         WalletSnapshot before, WalletSnapshot after, String idempotentKey, String remark) {
        DistributionWalletLog walletLog = new DistributionWalletLog();
        walletLog.setDistributionId(distributionId);
        walletLog.setBizType(logType.name());
        walletLog.setBizSn(bizSn);
        walletLog.setChangeAmount(changeAmount);
        walletLog.setBeforeCanRebate(before.canRebate);
        walletLog.setAfterCanRebate(after.canRebate);
        walletLog.setBeforeCommissionFrozen(before.commissionFrozen);
        walletLog.setAfterCommissionFrozen(after.commissionFrozen);
        walletLog.setBeforeWithdrawalFrozen(before.withdrawalFrozen);
        walletLog.setAfterWithdrawalFrozen(after.withdrawalFrozen);
        walletLog.setBeforeWithdrawnTotal(before.withdrawnTotal);
        walletLog.setAfterWithdrawnTotal(after.withdrawnTotal);
        walletLog.setIdempotentKey(idempotentKey);
        walletLog.setRemark(remark);
        distributionWalletLogMapper.insert(walletLog);
    }

    /**
     * Double 转 BigDecimal，null 视为零。
     *
     * @param value 原始值
     * @return BigDecimal
     */
    private static BigDecimal decimal(Double value) {
        return value == null ? BigDecimal.ZERO : BigDecimal.valueOf(value);
    }

    /**
     * BigDecimal 空值转零。
     *
     * @param value 原始值
     * @return 非空金额
     */
    private static BigDecimal decimal(BigDecimal value) {
        return value == null ? BigDecimal.ZERO : value;
    }

    /**
     * 钱包余额快照，用于记录资金变动前后的余额。
     */
    private static final class WalletSnapshot {
        private final BigDecimal canRebate;
        private final BigDecimal commissionFrozen;
        private final BigDecimal withdrawalFrozen;
        private final BigDecimal withdrawnTotal;

        /**
         * 构造钱包余额快照。
         *
         * @param canRebate        可提现余额
         * @param commissionFrozen 佣金冻结金额
         * @param withdrawalFrozen 提现冻结金额
         * @param withdrawnTotal   累计已提现金额
         */
        private WalletSnapshot(BigDecimal canRebate, BigDecimal commissionFrozen, BigDecimal withdrawalFrozen, BigDecimal withdrawnTotal) {
            this.canRebate = canRebate;
            this.commissionFrozen = commissionFrozen;
            this.withdrawalFrozen = withdrawalFrozen;
            this.withdrawnTotal = withdrawnTotal;
        }

        /**
         * 从分销员实体构建钱包余额快照。
         *
         * @param distribution 分销员
         * @return 钱包快照
         */
        private static WalletSnapshot from(Distribution distribution) {
            return new WalletSnapshot(
                    DistributionCommissionCalculator.toMoney(decimal(distribution.getCanRebate())),
                    DistributionCommissionCalculator.toMoney(decimal(distribution.getCommissionFrozen())),
                    DistributionCommissionCalculator.toMoney(decimal(distribution.getWithdrawalFrozen())),
                    DistributionCommissionCalculator.toMoney(decimal(distribution.getWithdrawnTotal()))
            );
        }
    }

    private enum WalletMutation {
        FREEZE_COMMISSION {
            @Override
            void apply(Distribution distribution, BigDecimal amount) {
                // 佣金冻结：待结算增加，累计收益同步增加
                distribution.setCommissionFrozen(add(distribution.getCommissionFrozen(), amount).doubleValue());
                distribution.setRebateTotal(add(distribution.getRebateTotal(), amount).doubleValue());
            }
        },
        SETTLE_COMMISSION {
            @Override
            void apply(Distribution distribution, BigDecimal amount) {
                // 佣金结算：冻结转可提现
                if (decimal(distribution.getCommissionFrozen()).compareTo(amount) < 0) {
                    throw new ServiceException(ResultCode.ERROR);
                }
                distribution.setCommissionFrozen(subtract(distribution.getCommissionFrozen(), amount).doubleValue());
                distribution.setCanRebate(add(distribution.getCanRebate(), amount).doubleValue());
            }
        },
        REFUND_COMMISSION {
            @Override
            void apply(Distribution distribution, BigDecimal amount) {
                // 佣金退回：优先扣减冻结，不足部分从可提现余额扣除
                BigDecimal commissionFrozen = decimal(distribution.getCommissionFrozen());
                if (commissionFrozen.compareTo(amount) >= 0) {
                    distribution.setCommissionFrozen(subtract(distribution.getCommissionFrozen(), amount).doubleValue());
                } else {
                    BigDecimal remain = amount.subtract(commissionFrozen);
                    distribution.setCommissionFrozen(0D);
                    distribution.setCanRebate(subtract(distribution.getCanRebate(), remain).doubleValue());
                }
                distribution.setRebateTotal(subtract(distribution.getRebateTotal(), amount).doubleValue());
            }
        },
        APPLY_WITHDRAW {
            @Override
            void apply(Distribution distribution, BigDecimal amount) {
                // 提现申请：可提现转提现冻结
                if (decimal(distribution.getCanRebate()).compareTo(amount) < 0) {
                    throw new ServiceException(ResultCode.WALLET_WITHDRAWAL_INSUFFICIENT);
                }
                distribution.setCanRebate(subtract(distribution.getCanRebate(), amount).doubleValue());
                distribution.setWithdrawalFrozen(add(distribution.getWithdrawalFrozen(), amount));
            }
        },
        APPROVE_WITHDRAW {
            @Override
            void apply(Distribution distribution, BigDecimal amount) {
                // 提现通过：冻结转已提现累计
                if (decimal(distribution.getWithdrawalFrozen()).compareTo(amount) < 0) {
                    throw new ServiceException(ResultCode.ERROR);
                }
                distribution.setWithdrawalFrozen(subtract(distribution.getWithdrawalFrozen(), amount));
                distribution.setWithdrawnTotal(add(distribution.getWithdrawnTotal(), amount));
            }
        },
        REJECT_WITHDRAW {
            @Override
            void apply(Distribution distribution, BigDecimal amount) {
                // 提现驳回：冻结退回可提现
                if (decimal(distribution.getWithdrawalFrozen()).compareTo(amount) < 0) {
                    throw new ServiceException(ResultCode.ERROR);
                }
                distribution.setWithdrawalFrozen(subtract(distribution.getWithdrawalFrozen(), amount));
                distribution.setCanRebate(add(distribution.getCanRebate(), amount).doubleValue());
            }
        };

        abstract void apply(Distribution distribution, BigDecimal amount);

        /** Double 与 BigDecimal 相加。 */
        private static BigDecimal add(Double value, BigDecimal amount) {
            return DistributionCommissionCalculator.toMoney(decimal(value).add(amount));
        }

        /** BigDecimal 与 BigDecimal 相加。 */
        private static BigDecimal add(BigDecimal value, BigDecimal amount) {
            return DistributionCommissionCalculator.toMoney(decimal(value).add(amount));
        }

        /** Double 与 BigDecimal 相减。 */
        private static BigDecimal subtract(Double value, BigDecimal amount) {
            return DistributionCommissionCalculator.toMoney(decimal(value).subtract(amount));
        }

        /** BigDecimal 与 BigDecimal 相减。 */
        private static BigDecimal subtract(BigDecimal value, BigDecimal amount) {
            return DistributionCommissionCalculator.toMoney(decimal(value).subtract(amount));
        }
    }
}
