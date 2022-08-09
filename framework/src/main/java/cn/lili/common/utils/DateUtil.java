package cn.lili.common.utils;

import java.text.SimpleDateFormat;
import java.time.*;
import java.util.*;

/**
 * 日期相关的操作
 *
 * @author Chopper
 */
public class DateUtil {

    public static final String STANDARD_FORMAT = "yyyy-MM-dd HH:mm:ss";

    public static final String STANDARD_DATE_FORMAT = "yyyy-MM-dd";

    public static final String STANDARD_DATE_NO_UNDERLINE_FORMAT = "yyyyMMdd";

    public static final String FULL_DATE = "yyyyMMddHHmmss";


    /**
     * 当天的开始时间
     *
     * @return 今天开始时间
     */
    public static Long getDayOfStart() {
        return DateUtil.getDateline()/(60*24*60);
    }
    /**
     * 指定日的开始时间
     *
     * @return 指定日时间
     */
    public static Long getDayOfStart(Date date) {
        return date.getTime()/(60*24*60);
    }

    /**
     * 当天的开始时间
     *
     * @return 今天开始时间
     */
    public static Date startOfTodDayTime() {
        Calendar calendar = Calendar.getInstance();
        calendar.set(Calendar.HOUR_OF_DAY, 0);
        calendar.set(Calendar.MINUTE, 0);
        calendar.set(Calendar.SECOND, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        return calendar.getTime();
    }

    /**
     * 当天的开始时间
     *
     * @param date 时间
     * @return 根据传入的时间获取开始时间
     */
    public static Date startOfTodDayTime(Date date) {
        Calendar calendar = Calendar.getInstance();
        calendar.set(Calendar.HOUR_OF_DAY, 0);
        calendar.set(Calendar.MINUTE, 0);
        calendar.set(Calendar.SECOND, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        return calendar.getTime();
    }

    /**
     * 当天的开始时间
     *
     * @return 今天开始时间
     */
    public static long startOfTodDay() {
        Calendar calendar = Calendar.getInstance();
        calendar.set(Calendar.HOUR_OF_DAY, 0);
        calendar.set(Calendar.MINUTE, 0);
        calendar.set(Calendar.SECOND, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        Date date = calendar.getTime();
        return date.getTime() / 1000;
    }

    /**
     * 当天的结束时间
     *
     * @return 今天结束时间
     */
    public static Date endOfDate() {
        Calendar calendar = Calendar.getInstance();
        calendar.set(Calendar.HOUR_OF_DAY, 23);
        calendar.set(Calendar.MINUTE, 59);
        calendar.set(Calendar.SECOND, 59);
        calendar.set(Calendar.MILLISECOND, 999);
        return calendar.getTime();
    }

    /**
     * 当天的结束时间
     *
     * @param date 传入日期
     * @return 获得传入日期当天结束时间
     */
    public static Date endOfDate(Date date) {
        if (date == null) {
            date = new Date();
        }
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(date);
        calendar.set(Calendar.HOUR_OF_DAY, 23);
        calendar.set(Calendar.MINUTE, 59);
        calendar.set(Calendar.SECOND, 59);
        calendar.set(Calendar.MILLISECOND, 999);
        return calendar.getTime();
    }

    /**
     * 某天的年月日
     *
     * @param dayUntilNow 距今多少天以前
     * @return 年月日map key为 year month day
     */
    public static Map<String, Object> getYearMonthAndDay(int dayUntilNow) {

        Map<String, Object> map = new HashMap<String, Object>(3);
        Calendar calendar = Calendar.getInstance();
        calendar.set(Calendar.HOUR_OF_DAY, 0);
        calendar.set(Calendar.MINUTE, 0);
        calendar.set(Calendar.SECOND, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        calendar.add(Calendar.DATE, -dayUntilNow);
        map.put("year", calendar.get(Calendar.YEAR));
        map.put("month", calendar.get(Calendar.MONTH) + 1);
        map.put("day", calendar.get(Calendar.DAY_OF_MONTH));
        return map;
    }

    /**
     * 将一个字符串转换成日期格式
     *
     * @param date    字符串日期
     * @param pattern 日期格式
     * @return date
     */
    public static Date toDate(String date, String pattern) {
        if ("".equals("" + date)) {
            return null;
        }
        if (pattern == null) {
            pattern = STANDARD_DATE_FORMAT;
        }
        SimpleDateFormat sdf = new SimpleDateFormat(pattern, Locale.ENGLISH);
        Date newDate = new Date();
        try {
            newDate = sdf.parse(date);

        } catch (Exception ex) {
            ex.printStackTrace();
        }
        return newDate;
    }

    /**
     * 获取上个月的开始结束时间
     *
     * @return 上个月的开始结束时间
     */
    public static Long[] getLastMonth() {
        //取得系统当前时间
        Calendar cal = Calendar.getInstance();
        int year = cal.get(Calendar.YEAR);
        int month = cal.get(Calendar.MONTH) + 1;

        //取得系统当前时间所在月第一天时间对象
        cal.set(Calendar.DAY_OF_MONTH, 1);

        //日期减一,取得上月最后一天时间对象
        cal.add(Calendar.DAY_OF_MONTH, -1);

        //输出上月最后一天日期
        int day = cal.get(Calendar.DAY_OF_MONTH);

        String months = "";
        String days = "";

        if (month > 1) {
            month--;
        } else {
            year--;
            month = 12;
        }
        if (String.valueOf(month).length() <= 1) {
            months = "0" + month;
        } else {
            months = String.valueOf(month);
        }
        if (String.valueOf(day).length() <= 1) {
            days = "0" + day;
        } else {
            days = String.valueOf(day);
        }
        String firstDay = "" + year + "-" + months + "-01";
        String lastDay = "" + year + "-" + months + "-" + days + " 23:59:59";

        Long[] lastMonth = new Long[2];
        lastMonth[0] = DateUtil.getDateline(firstDay);
        lastMonth[1] = DateUtil.getDateline(lastDay, STANDARD_FORMAT);

        return lastMonth;
    }

    /**
     * 把日期转换成字符串型
     *
     * @param date 日期
     * @return 字符串时间
     */
    public static String toString(Date date) {
        return toString(date, STANDARD_FORMAT);
    }

    /**
     * 把日期转换成字符串型
     *
     * @param date 日期
     * @return 字符串时间
     */
    public static String toString(Long date) {
        return toString(date, STANDARD_FORMAT);
    }

    /**
     * 把日期转换成字符串型
     *
     * @param date    日期
     * @param pattern 类型
     * @return 字符串时间
     */
    public static String toString(Date date, String pattern) {
        if (date == null) {
            return "";
        }
        if (pattern == null) {
            pattern = STANDARD_DATE_FORMAT;
        }
        String dateString = "";
        SimpleDateFormat sdf = new SimpleDateFormat(pattern);
        try {
            dateString = sdf.format(date);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        return dateString;
    }

    /**
     * 时间戳转换成时间类型
     *
     * @param time    时间戳
     * @param pattern 格式
     * @return 字符串时间
     */
    public static String toString(Long time, String pattern) {
        if (time > 0) {
            if (time.toString().length() == 10) {
                time = time * 1000;
            }
            Date date = new Date(time);
            return DateUtil.toString(date, pattern);
        }
        return "";
    }

    /**
     * 判断当前时间是否在某个时间范围
     *
     * @param start 开始时间，以秒为单位的时间戳
     * @param end   结束时间，以秒为单位的时间戳
     * @return 是否在范围内
     */
    public static boolean inRangeOf(long start, long end) {
        long now = getDateline();
        return start <= now && end >= now;
    }

    /**
     * 获取指定日期的时间戳
     *
     * @param date 指定日期
     * @return 时间戳
     */
    public static long getDateline(String date) {
        return Objects.requireNonNull(toDate(date, STANDARD_DATE_FORMAT)).getTime() / 1000;
    }

    /**
     * 获取当前时间的时间戳
     *
     * @return 时间戳
     */
    public static long getDateline() {
        return System.currentTimeMillis() / 1000;
    }

    /**
     * 获取当前时间格式化字符串
     *
     * @return 时间戳
     */
    public static String getCurrentDateStr(String format) {
        return toString(new Date(), format);
    }

    /**
     * 获取当前时间格式化字符串
     *
     * @return 格式化的时间
     */
    public static String getCurrentDateStr() {
        return toString(new Date(), FULL_DATE);
    }

    /**
     * 根据日期格式及日期获取时间戳
     *
     * @param date    日期
     * @param pattern 日期格式
     * @return 时间戳
     */
    public static long getDateline(String date, String pattern) {
        return Objects.requireNonNull(toDate(date, pattern)).getTime() / 1000;
    }

    /**
     * 获取几个月之前的日期时间戳
     *
     * @param beforeMonth 几个月之前
     * @return 时间戳
     */
    public static long getBeforeMonthDateline(int beforeMonth) {
        SimpleDateFormat format = new SimpleDateFormat(STANDARD_FORMAT);
        Calendar c = Calendar.getInstance();

        //过去一月
        c.setTime(new Date());
        c.add(Calendar.MONTH, (0 - beforeMonth));
        Date m = c.getTime();
        String mon = format.format(m);
        return getDateline(mon, STANDARD_FORMAT);
    }

    /**
     * 获取当前天的结束时间
     *
     * @return 当前天的结束时间
     */
    public static Date getCurrentDayEndTime() {
        Calendar cal = Calendar.getInstance();
        cal.set(Calendar.HOUR_OF_DAY, 0);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);
        cal.set(Calendar.DATE, cal.get(Calendar.DATE) + 1);
        cal.set(Calendar.SECOND, cal.get(Calendar.SECOND) - 1);
        return cal.getTime();
    }

    /**
     * 获取延时时间（秒）
     *
     * @param startTime 开始时间
     * @return 延时时间（秒）
     */
    public static Integer getDelayTime(Long startTime) {
        int time = Math.toIntExact((startTime - System.currentTimeMillis()) / 1000);
        //如果时间为负数则改为一秒后执行
        if (time <= 0) {
            time = 1;
        }
        return time;
    }

    /**
     * 获取某年某月开始时间
     *
     * @param year  年
     * @param month 月
     * @return 开始时间
     */
    public static Date getBeginTime(int year, int month) {
        YearMonth yearMonth = YearMonth.of(year, month);
        LocalDate localDate = yearMonth.atDay(1);
        LocalDateTime startOfDay = localDate.atStartOfDay();
        ZonedDateTime zonedDateTime = startOfDay.atZone(ZoneId.of("Asia/Shanghai"));

        return Date.from(zonedDateTime.toInstant());

    }

    /**
     * 获取某年某月结束时间
     *
     * @param year  年
     * @param month 月
     * @return 结束时间
     */
    public static Date getEndTime(int year, int month) {
        YearMonth yearMonth = YearMonth.of(year, month);
        LocalDate endOfMonth = yearMonth.atEndOfMonth();
        LocalDateTime localDateTime = endOfMonth.atTime(23, 59, 59, 999);
        ZonedDateTime zonedDateTime = localDateTime.atZone(ZoneId.of("Asia/Shanghai"));
        return Date.from(zonedDateTime.toInstant());
    }
}
