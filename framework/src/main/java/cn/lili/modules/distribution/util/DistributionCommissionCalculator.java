package cn.lili.modules.distribution.util;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * 分销佣金计算工具
 *
 * @author Bulbasaur
 * @since 2026-08-04
 */
public final class DistributionCommissionCalculator {

    public static final int MONEY_SCALE = 2;
    public static final int RATE_SCALE = 4;
    public static final RoundingMode ROUNDING = RoundingMode.HALF_UP;

    private DistributionCommissionCalculator() {
    }

    /**
     * 金额统一保留两位小数。
     *
     * @param value 原始金额
     * @return 格式化后的金额，null 视为 0
     */
    public static BigDecimal toMoney(BigDecimal value) {
        if (value == null) {
            return BigDecimal.ZERO.setScale(MONEY_SCALE, ROUNDING);
        }
        return value.setScale(MONEY_SCALE, ROUNDING);
    }

    /**
     * 比例统一保留四位小数。
     *
     * @param value 原始比例
     * @return 格式化后的比例，null 视为 0
     */
    public static BigDecimal toRate(BigDecimal value) {
        if (value == null) {
            return BigDecimal.ZERO.setScale(RATE_SCALE, ROUNDING);
        }
        return value.setScale(RATE_SCALE, ROUNDING);
    }

    /**
     * 计算订单项佣金池（单品佣金 × 数量）。
     *
     * @param unitCommission 单品固定佣金
     * @param quantity       购买数量
     * @return 佣金池金额
     */
    public static BigDecimal calculatePool(BigDecimal unitCommission, int quantity) {
        if (unitCommission == null || quantity <= 0) {
            return BigDecimal.ZERO.setScale(MONEY_SCALE, ROUNDING);
        }
        return toMoney(unitCommission.multiply(BigDecimal.valueOf(quantity)));
    }

    /**
     * 按等级比例拆分佣金池为一级（商品）与二级（邀请）佣金。
     *
     * @param pool        佣金池
     * @param directRate  一级商品佣金比例（0～1 小数，0.1 表示 10%）
     * @param inviteRate  二级邀请佣金比例（0～1 小数，0.1 表示 10%）
     * @return [一级佣金, 二级佣金]
     */
    public static BigDecimal[] splitPool(BigDecimal pool, BigDecimal directRate, BigDecimal inviteRate) {
        BigDecimal normalizedPool = toMoney(pool);
        if (normalizedPool.compareTo(BigDecimal.ZERO) <= 0) {
            return new BigDecimal[]{BigDecimal.ZERO.setScale(MONEY_SCALE, ROUNDING),
                    BigDecimal.ZERO.setScale(MONEY_SCALE, ROUNDING)};
        }
        BigDecimal direct = toMoney(normalizedPool.multiply(rateFraction(directRate)));
        BigDecimal invite = toMoney(normalizedPool.multiply(rateFraction(inviteRate)));
        return new BigDecimal[]{direct, invite};
    }

    /**
     * 计算直属分销员商品佣金（佣金池 × 等级商品佣金比例）。
     * 比例为 0～1 小数，0.1 表示 10%。
     */
    public static BigDecimal calculateDirectCommission(BigDecimal pool, BigDecimal directRate) {
        return splitPool(pool, directRate, BigDecimal.ZERO)[0];
    }

    /**
     * 将等级比例转为结算用小数。入参是 0～1 小数，0.1 表示 10%。
     * null / 负数按 0 处理；大于 1 的脏数据不截断、不按百分数换算，保持原值相乘。
     */
    private static BigDecimal rateFraction(BigDecimal rate) {
        BigDecimal normalizedRate = toRate(rate);
        if (normalizedRate.compareTo(BigDecimal.ZERO) < 0) {
            return BigDecimal.ZERO.setScale(RATE_SCALE, ROUNDING);
        }
        return normalizedRate;
    }
}
