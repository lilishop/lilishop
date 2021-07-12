package cn.lili.timetask.handler.impl.statistics;

import cn.lili.common.cache.Cache;
import cn.lili.common.cache.CachePrefix;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.config.properties.StatisticsProperties;
import cn.lili.modules.statistics.model.vo.OnlineMemberVO;
import cn.lili.timetask.handler.EveryHourExecute;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

/**
 * 实时在线人数统计
 *
 * @author Chopper
 * @date 2021-02-21 09:47
 */
@Component
public class OnlineMemberStatistics implements EveryHourExecute {

    /**
     * 缓存
     */
    @Autowired
    private Cache cache;
    /**
     * 统计小时
     */
    @Autowired
    private StatisticsProperties statisticsProperties;


    @Override
    public void execute() {

        Calendar calendar = Calendar.getInstance();

        Object object = cache.get(CachePrefix.ONLINE_MEMBER.getPrefix());
        List<OnlineMemberVO> onlineMemberVOS;
        if (object == null) {
            onlineMemberVOS = new ArrayList<>();
        } else {
            onlineMemberVOS = (List<OnlineMemberVO>) object;
        }

        //过滤 有效统计时间
        calendar.set(Calendar.HOUR_OF_DAY, calendar.get(Calendar.HOUR_OF_DAY) - statisticsProperties.getOnlineMember());
        calendar.set(Calendar.MINUTE, 0);
        calendar.set(Calendar.SECOND, 0);
        calendar.set(Calendar.MILLISECOND, 0);

        Calendar finalCalendar = calendar;
        onlineMemberVOS = onlineMemberVOS.stream()
                .filter(onlineMemberVO -> onlineMemberVO.getDate().after(finalCalendar.getTime()))
                .collect(Collectors.toList());

        //计入新数据
        calendar = Calendar.getInstance();
        calendar.set(Calendar.MINUTE, 0);
        calendar.set(Calendar.SECOND, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        onlineMemberVOS.add(new OnlineMemberVO(calendar.getTime(), cache.keys(CachePrefix.ACCESS_TOKEN.getPrefix(UserEnums.MEMBER) + "*").size()));

        //写入缓存
        cache.put(CachePrefix.ONLINE_MEMBER.getPrefix(), onlineMemberVOS);
    }


    /**
     * 手动设置某一时间，活跃人数
     *
     * @param time 时间
     * @param num  人数
     */
    public void execute(Date time, Integer num) {

        Object object = cache.get(CachePrefix.ONLINE_MEMBER.getPrefix());
        List<OnlineMemberVO> onlineMemberVOS;
        if (object == null) {
            onlineMemberVOS = new ArrayList<>();
        } else {
            onlineMemberVOS = (List<OnlineMemberVO>) object;
        }

        Calendar calendar = Calendar.getInstance();
        calendar.setTime(time);
        //过滤 有效统计时间
        calendar.set(Calendar.MINUTE, 0);
        calendar.set(Calendar.SECOND, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        calendar.set(Calendar.HOUR_OF_DAY, calendar.get(Calendar.HOUR_OF_DAY) - 48);

        Calendar finalCalendar = calendar;
        onlineMemberVOS = onlineMemberVOS.stream()
                .filter(onlineMemberVO -> onlineMemberVO.getDate().after(finalCalendar.getTime()))
                .collect(Collectors.toList());
        onlineMemberVOS.add(new OnlineMemberVO(time, num));

        //写入缓存
        cache.put(CachePrefix.ONLINE_MEMBER.getPrefix(), onlineMemberVOS);
    }

}
