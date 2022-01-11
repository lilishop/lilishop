package cn.lili.common.utils;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Arrays;

/**
 * 金额计算工具
 *
 * @author Bulbasaur
 * @since 2021/7/9 1:40 上午
 */
public final class CurrencyUtil {
    /**
     * 默认除法运算精度
     */
    private static final int DEF_DIV_SCALE = 2;

    /**
     * 这个类不能实例化
     */
    private CurrencyUtil() {
    }

    /**
     * 提供精确的加法运算。
     *
     * @return 累加之和
     */
    public static Double add(double... params) {
        BigDecimal result = new BigDecimal("0");
        for (double param : params) {
            BigDecimal bigParam = BigDecimal.valueOf(param);
            result = result.add(bigParam).setScale(2, RoundingMode.HALF_UP);
        }
        return result.doubleValue();
    }
    /**
     * 提供精确的加法运算。
     *
     * @return 累加之和
     */
    public static Double sub(double... params) {
        BigDecimal result = BigDecimal.valueOf(params[0]);
        params = Arrays.stream(params).skip(1).toArray();
        for (double param : params) {
            BigDecimal bigParam = BigDecimal.valueOf(param);
            result = result.subtract(bigParam).setScale(2, RoundingMode.HALF_UP);
        }
        return result.doubleValue();
    }

    /**
     * 提供精确的乘法运算。
     *
     * @param v1 被乘数
     * @param v2 乘数
     * @return 两个参数的积
     */
    public static Double mul(double v1, double v2) {
        BigDecimal b1 = BigDecimal.valueOf(v1);
        BigDecimal b2 = BigDecimal.valueOf(v2);
        return b1.multiply(b2).setScale(2, RoundingMode.HALF_UP).doubleValue();
    }

    /**
     * 提供精确的乘法运算。
     *
     * @param v1    被乘数
     * @param v2    乘数
     * @param scale 表示表示需要精确到小数点以后几位。
     * @return 两个参数的积
     */
    public static Double mul(double v1, double v2, int scale) {
        if (scale < 0) {
            throw new IllegalArgumentException(
                    "The scale must be a positive integer or zero");
        }
        BigDecimal b1 = BigDecimal.valueOf(v1);
        BigDecimal b2 = BigDecimal.valueOf(v2);
        return b1.multiply(b2).setScale(scale, RoundingMode.HALF_UP).doubleValue();
    }

    /**
     * 提供（相对）精确的除法运算，当发生除不尽的情况时， 精确到小数点以后10位，以后的数字四舍五入。
     *
     * @param v1 被除数
     * @param v2 除数
     * @return 两个参数的商
     */
    public static double div(double v1, double v2) {
        return div(v1, v2, DEF_DIV_SCALE);
    }

    /**
     * 提供（相对）精确的除法运算。 当发生除不尽的情况时，由scale参数指定精度，以后的数字四舍五入。
     *
     * @param v1    被除数
     * @param v2    除数
     * @param scale 表示表示需要精确到小数点以后几位。
     * @return 两个参数的商
     */
    public static double div(double v1, double v2, int scale) {
        if (scale < 0) {
            throw new IllegalArgumentException(
                    "The scale must be a positive integer or zero");
        }
        //如果被除数等于0，则返回0
        if (v2 == 0) {
            return 0;
        }
        BigDecimal b1 = BigDecimal.valueOf(v1);
        BigDecimal b2 = BigDecimal.valueOf(v2);
        return b1.divide(b2, scale, RoundingMode.HALF_UP).doubleValue();
    }

    /**
     * 金额转分
     *
     * @param money 金额
     * @return 转换单位为分
     */
    public static Integer fen(Double money) {
        double price = mul(money, 100);
        return (int) price;
    }

    /**
     * 金额转分
     *
     * @param money 金额
     * @return double类型分
     */
    public static double reversalFen(Double money) {
        return div(money, 100);
    }
}