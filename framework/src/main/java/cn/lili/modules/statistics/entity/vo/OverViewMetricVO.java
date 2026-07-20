package cn.lili.modules.statistics.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 带环比的统计指标
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class OverViewMetricVO {

    @Schema(description = "当前周期数值")
    private Double current;

    @Schema(description = "上一周期数值")
    private Double lastPeriod;

    @Schema(description = "环比百分比字符串，如 +50%、-33%")
    private String rate;

    @Schema(description = "涨跌方向：UP/DOWN/FLAT")
    private String trend;

    /**
     * 根据本期、上期构建环比指标
     * 公式：(本期-上期)/上期*100%；上期为0时本期>0记+100%，本期=0记0%
     */
    public static OverViewMetricVO of(Double current, Double lastPeriod) {
        double cur = current == null ? 0D : current;
        double last = lastPeriod == null ? 0D : lastPeriod;
        OverViewMetricVO vo = new OverViewMetricVO();
        vo.setCurrent(cur);
        vo.setLastPeriod(last);

        double diff = cur - last;
        String rate;
        String trend;
        if (last == 0D) {
            if (cur == 0D) {
                rate = "0%";
                trend = "FLAT";
            } else {
                rate = "+100%";
                trend = "UP";
            }
        } else {
            double percent = diff / last * 100;
            // 保留整数百分比展示
            long rounded = Math.round(percent);
            if (rounded > 0) {
                rate = "+" + rounded + "%";
                trend = "UP";
            } else if (rounded < 0) {
                rate = rounded + "%";
                trend = "DOWN";
            } else {
                rate = "0%";
                trend = "FLAT";
            }
        }
        vo.setRate(rate);
        vo.setTrend(trend);
        return vo;
    }

    public static OverViewMetricVO of(Long current, Long lastPeriod) {
        return of(current == null ? 0D : current.doubleValue(),
                lastPeriod == null ? 0D : lastPeriod.doubleValue());
    }
}
