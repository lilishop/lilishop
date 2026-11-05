package cn.lili.modules.member.util;

import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.YearMonth;
import java.time.temporal.ChronoUnit;

/**
 * 权益配置和周期计算；周期按中国业务日期计算，不追发历史周期。
 *
 * @author Bulbasaur
 * @since 2026-11-04
 */
public final class MemberBenefitRules {
    /**
     * 规则工具类不允许实例化。
     */
    private MemberBenefitRules() { }

    /**
     * 按允许的格式、块、链接及图片白名单清理富文本，过滤不允许的标签与属性。
     *
     * @param html 原始说明富文本，可为null
     * @return 清理后的HTML，空值按空字符串处理
     */
    public static String description(String html) {
        return org.owasp.html.Sanitizers.FORMATTING.and(org.owasp.html.Sanitizers.BLOCKS)
                .and(org.owasp.html.Sanitizers.LINKS).and(org.owasp.html.Sanitizers.IMAGES).sanitize(html == null ? "" : html);
    }

    /**
     * 解析权益扩展配置，空白配置返回空JSON对象。
     *
     * @param json 原始JSON配置
     * @return 配置对象
     * @throws ServiceException 非空配置无法解析为JSON对象
     */
    public static JSONObject config(String json) {
        try {
            return json == null || json.isBlank() ? new JSONObject() : JSONUtil.parseObj(json);
        } catch (Exception e) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "权益配置不是合法JSON");
        }
    }

    /**
     * 读取并校验配置数值的闭区间范围及有效小数位，不对超精度数值执行舍入。
     *
     * @param config 配置对象
     * @param key 待读取字段名
     * @param min 允许的最小值，包含边界
     * @param max 允许的最大值，包含边界
     * @param scale 允许的最大小数位，0表示整数，末尾无效零不计精度
     * @return 合法数值
     * @throws ServiceException 字段缺失、非数字、越界或超出精度
     */
    public static BigDecimal number(JSONObject config, String key, BigDecimal min, BigDecimal max, int scale) {
        try {
            BigDecimal value = new BigDecimal(String.valueOf(config.get(key)));
            if (value.compareTo(min) < 0 || value.compareTo(max) > 0 || value.stripTrailingZeros().scale() > scale) {
                throw new IllegalArgumentException();
            }
            return value;
        } catch (Exception e) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, key + "范围为" + min + "-" + max + "，最多" + scale + "位小数");
        }
    }

    /**
     * 校验券礼包发放规则，缺省为ONCE一次性；MONTHLY要求1～31的整数发放日，PERIOD要求1～12的整数月数。
     *
     * @param config 权益扩展配置
     * @throws ServiceException 发放类型、日期或周期月数非法
     */
    public static void validateSchedule(JSONObject config) {
        String rule = config.getStr("grantRule", "ONCE");
        switch (rule) {
            case "ONCE": break;
            case "MONTHLY": number(config, "grantDay", BigDecimal.ONE, BigDecimal.valueOf(31), 0); break;
            case "PERIOD": number(config, "periodMonths", BigDecimal.ONE, BigDecimal.valueOf(12), 0); break;
            default: throw new ServiceException(ResultCode.PARAMS_ERROR, "发放规则非法");
        }
    }

    /**
     * 根据业务日期返回当前应发周期键，不枚举或追补尚未生成的历史周期；调用方先校验配置。
     * ONCE返回固定键；MONTHLY仅在本月到期日已到且不早于资格开始日期时返回，缺失日期取月末。
     * PERIOD从资格开始日期按自然月分段，首期从资格开始时生效，使用当前所在区间编号。
     *
     * @param config 已校验的发放规则配置
     * @param reached 等级权益初始化或付费订单生效的业务日期
     * @param today 当前业务日期，由调用方按上海时区转换
     * @return ONCE、MONTH:年月或PERIOD:月数:区间编号；资格尚未开始或本月尚未到期时为null
     */
    public static String cycle(JSONObject config, LocalDate reached, LocalDate today) {
        if (today.isBefore(reached)) return null;
        String rule = config.getStr("grantRule", "ONCE");
        if ("ONCE".equals(rule)) return "ONCE";
        if ("MONTHLY".equals(rule)) {
            int day = Math.min(config.getInt("grantDay"), today.lengthOfMonth());
            LocalDate due = today.withDayOfMonth(day);
            return today.isBefore(due) || due.isBefore(reached) ? null : "MONTH:" + YearMonth.from(today);
        }
        int months = config.getInt("periodMonths");
        long elapsed = ChronoUnit.MONTHS.between(YearMonth.from(reached), YearMonth.from(today));
        long index = elapsed / months;
        if (today.isBefore(reached.plusMonths(index * months))) index--;
        return index < 0 ? null : "PERIOD:" + months + ":" + index;
    }
}
