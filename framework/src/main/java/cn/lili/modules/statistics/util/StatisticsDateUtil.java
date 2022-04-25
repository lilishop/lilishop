package cn.lili.modules.statistics.util;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.StringUtils;
import cn.lili.modules.statistics.entity.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.entity.enums.SearchTypeEnum;

import java.util.Calendar;
import java.util.Date;

/**
 * 统计缓存后缀工具
 *
 * @author Chopper
 * @since 2021-01-15 15:30
 */
public class StatisticsDateUtil {


    /**
     * 快捷搜索，得到开始时间和结束时间
     *
     * @param searchTypeEnum
     * @return
     */
    public static Date[] getDateArray(SearchTypeEnum searchTypeEnum) {
        Date[] dateArray = new Date[2];

        Calendar calendar = Calendar.getInstance();
        //时间归到今天凌晨0点
        calendar.set(Calendar.HOUR_OF_DAY, 0);
        calendar.set(Calendar.MINUTE, 0);
        calendar.set(Calendar.SECOND, 0);
        calendar.set(Calendar.MILLISECOND, 0);

        switch (searchTypeEnum) {
            case TODAY:
                dateArray[0] = calendar.getTime();

                calendar.set(Calendar.HOUR_OF_DAY, +24);
                calendar.set(Calendar.MILLISECOND, -1);
                dateArray[1] = calendar.getTime();
                break;

            case YESTERDAY:
                //获取昨天
                calendar.set(Calendar.HOUR_OF_DAY, -24);
                dateArray[0] = calendar.getTime();

                //昨天结束时间
                calendar.set(Calendar.HOUR_OF_DAY, +24);
                calendar.set(Calendar.MILLISECOND, -1);
                dateArray[1] = calendar.getTime();
                break;
            case LAST_SEVEN:
                calendar.set(Calendar.HOUR_OF_DAY, -24 * 7);
                dateArray[0] = calendar.getTime();


                calendar.set(Calendar.HOUR_OF_DAY, +24 * 7);
                calendar.set(Calendar.MILLISECOND, -1);
                //获取过去七天
                dateArray[1] = calendar.getTime();
                break;
            case LAST_THIRTY:
                //获取最近三十天
                calendar.set(Calendar.HOUR_OF_DAY, -24 * 30);
                dateArray[0] = calendar.getTime();


                calendar.set(Calendar.HOUR_OF_DAY, +24 * 30);
                calendar.set(Calendar.MILLISECOND, -1);
                //获取过去七天
                dateArray[1] = calendar.getTime();
                break;
            default:
                throw new ServiceException(ResultCode.ERROR);
        }
        return dateArray;
    }


    /**
     * 获取年月获取开始结束时间
     *
     * @param year  年
     * @param month 月
     * @return 返回时间
     */
    public static Date[] getDateArray(Integer year, Integer month) {
        Date[] dateArray = new Date[2];

        Calendar calendar = Calendar.getInstance();
        calendar.set(year, month, 0);
        dateArray[1] = calendar.getTime();
        calendar.set(Calendar.MONTH, calendar.get(Calendar.MONTH) - 1);
        dateArray[0] = calendar.getTime();
        return dateArray;
    }

    /**
     * 根据搜索参数获取搜索开始结束时间
     *
     * @param statisticsQueryParam
     * @return
     */
    public static Date[] getDateArray(StatisticsQueryParam statisticsQueryParam) {
        //如果快捷搜搜哦
        if (StringUtils.isNotEmpty(statisticsQueryParam.getSearchType())) {
            return getDateArray(SearchTypeEnum.valueOf(statisticsQueryParam.getSearchType()));
        }
        //按照年月查询
        else if (statisticsQueryParam.getMonth() != null && statisticsQueryParam.getYear() != null) {
            return getDateArray(statisticsQueryParam.getYear(), statisticsQueryParam.getMonth());
        }
        //默认查询当前月份
        else {
            Calendar calendar = Calendar.getInstance();
            return getDateArray(calendar.get(Calendar.YEAR), calendar.get(Calendar.MONTH) + 1);
        }
    }


    /**
     * 根据一个日期，获取这一天的开始时间和结束时间
     *
     * @param queryDate
     * @return
     */
    public static Date[] getDateArray(Date queryDate) {

        Date[] dateArray = new Date[2];
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(queryDate);
        //时间归到今天凌晨0点
        calendar.set(Calendar.HOUR_OF_DAY, 0);
        calendar.set(Calendar.MINUTE, 0);
        calendar.set(Calendar.SECOND, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        dateArray[0] = calendar.getTime();

        calendar.set(Calendar.DAY_OF_YEAR, calendar.get(Calendar.DAY_OF_YEAR) + 1);
        calendar.set(Calendar.SECOND, calendar.get(Calendar.SECOND) - 1);

        dateArray[1] = calendar.getTime();
        return dateArray;
    }

}
