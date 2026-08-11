package cn.lili.modules.distribution.support;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.modules.distribution.entity.dto.DistributionInviteeQueryDTO;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.util.Date;

/**
 * 邀请列表时间范围解析
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
public final class DistributionInviteRangeSupport {

    public static final String RANGE_ALL = "ALL";
    public static final String RANGE_TODAY = "TODAY";
    public static final String RANGE_YESTERDAY = "YESTERDAY";
    public static final String RANGE_LAST_7_DAYS = "LAST_7_DAYS";
    public static final String RANGE_CUSTOM = "CUSTOM";

    private DistributionInviteRangeSupport() {
    }

    /**
     * 从查询 DTO 解析邀请列表时间范围。
     *
     * @param query 查询参数
     * @return 时间范围
     */
    public static DateRange resolveRange(DistributionInviteeQueryDTO query) {
        if (query == null) {
            return DateRange.empty();
        }
        return resolveRange(query.getRangeType(), query.getStartTime(), query.getEndTime());
    }

    /**
     * 按范围类型解析邀请列表起止时间。
     *
     * @param rangeType 范围类型
     * @param startTime 自定义开始时间
     * @param endTime   自定义结束时间
     * @return 时间范围
     */
    public static DateRange resolveRange(String rangeType, Date startTime, Date endTime) {
        String type = CharSequenceUtil.blankToDefault(rangeType, RANGE_ALL).toUpperCase();
        return switch (type) {
            case RANGE_TODAY -> dayRange(LocalDate.now());
            case RANGE_YESTERDAY -> dayRange(LocalDate.now().minusDays(1));
            // 含今天共 7 天：今天往前推 6 天
            case RANGE_LAST_7_DAYS -> between(
                    LocalDate.now().minusDays(6).atStartOfDay(),
                    LocalDate.now().atTime(LocalTime.MAX));
            case RANGE_CUSTOM -> between(
                    toLocalDateTime(startTime),
                    toLocalDateTime(endTime));
            // ALL 或未识别类型：不限时间
            default -> DateRange.empty();
        };
    }

    public static class DateRange {
        private Date startTime;
        private Date endTime;

        /**
         * 创建无时间限制的空范围。
         */
        public static DateRange empty() {
            return new DateRange();
        }

        public Date getStartTime() {
            return startTime;
        }

        public void setStartTime(Date startTime) {
            this.startTime = startTime;
        }

        public Date getEndTime() {
            return endTime;
        }

        public void setEndTime(Date endTime) {
            this.endTime = endTime;
        }
    }

    /**
     * 构建指定自然日的起止时间范围。
     *
     * @param day 自然日
     * @return 时间范围
     */
    private static DateRange dayRange(LocalDate day) {
        return between(day.atStartOfDay(), day.atTime(LocalTime.MAX));
    }

    /**
     * 由 LocalDateTime 起止构建时间范围。
     *
     * @param start 开始时间
     * @param end   结束时间
     * @return 时间范围
     */
    private static DateRange between(LocalDateTime start, LocalDateTime end) {
        if (start == null && end == null) {
            return DateRange.empty();
        }
        // 允许仅传开始或结束，用于开放式区间筛选
        DateRange range = new DateRange();
        if (start != null) {
            range.setStartTime(toDate(start));
        }
        if (end != null) {
            range.setEndTime(toDate(end));
        }
        return range;
    }

    /**
     * Date 转 LocalDateTime。
     *
     * @param date 日期
     * @return 本地日期时间，date 为 null 时返回 null
     */
    private static LocalDateTime toLocalDateTime(Date date) {
        if (date == null) {
            return null;
        }
        return LocalDateTime.ofInstant(date.toInstant(), ZoneId.systemDefault());
    }

    /**
     * LocalDateTime 转 Date。
     *
     * @param dateTime 本地日期时间
     * @return 日期
     */
    private static Date toDate(LocalDateTime dateTime) {
        return Date.from(dateTime.atZone(ZoneId.systemDefault()).toInstant());
    }
}
