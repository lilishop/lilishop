package cn.lili.timetask.handler.impl.statistics;

import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.properties.StatisticsProperties;
import cn.lili.modules.statistics.entity.vo.OnlineMemberVO;
import cn.lili.timetask.handler.EveryHourExecute;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

/**
 * 实时在线人数统计
 *
 * @author Chopper
 * @since 2021-02-21 09:47
 */
@Component
public class OnlineMemberStatistics implements EveryHourExecute {

    /**
     * 缓存
     */
    @Autowired
    private Cache<List<OnlineMemberVO>> cache;
    /**
     * 统计小时
     */
    @Autowired
    private StatisticsProperties statisticsProperties;


    @Override
    public void execute() {

        Calendar calendar = Calendar.getInstance();

        List<OnlineMemberVO> onlineMemberVOS = cache.get(CachePrefix.ONLINE_MEMBER.getPrefix());

        if (onlineMemberVOS == null) {
            onlineMemberVOS = new ArrayList<>();
        }

        //过滤 有效统计时间
        calendar.set(Calendar.HOUR_OF_DAY, calendar.get(Calendar.HOUR_OF_DAY) - statisticsProperties.getOnlineMember());
        calendar.set(Calendar.MINUTE, 0);
        calendar.set(Calendar.SECOND, 0);
        calendar.set(Calendar.MILLISECOND, 0);

        Calendar finalCalendar = calendar;

        AtomicReference<Integer> lastNum = new AtomicReference<>(0);
        onlineMemberVOS = onlineMemberVOS.stream()
                .filter(onlineMemberVO -> {
                    if (onlineMemberVO.getDate().before(finalCalendar.getTime())) {
                        lastNum.set(onlineMemberVO.getNum());
                    }
                    return onlineMemberVO.getDate().after(finalCalendar.getTime());
                })
                .collect(Collectors.toList());

        //计入新数据
        calendar = Calendar.getInstance();
        calendar.set(Calendar.MINUTE, 0);
        calendar.set(Calendar.SECOND, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        onlineMemberVOS.add(new OnlineMemberVO(calendar.getTime(), cache.keys(CachePrefix.ACCESS_TOKEN.getPrefix(UserEnums.MEMBER) + "*").size(), lastNum.get()));

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

        List<OnlineMemberVO> onlineMemberVOS = cache.get(CachePrefix.ONLINE_MEMBER.getPrefix());

        if (onlineMemberVOS == null) {
            onlineMemberVOS = new ArrayList<>();
        }

        Calendar calendar = Calendar.getInstance();
        calendar.setTime(time);
        //过滤 有效统计时间
        calendar.set(Calendar.MINUTE, 0);
        calendar.set(Calendar.SECOND, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        calendar.set(Calendar.HOUR_OF_DAY, calendar.get(Calendar.HOUR_OF_DAY) - 48);

        onlineMemberVOS = onlineMemberVOS.stream()
                .filter(onlineMemberVO -> onlineMemberVO.getDate().after(calendar.getTime()))
                .collect(Collectors.toList());
        onlineMemberVOS.add(new OnlineMemberVO(time, num, num));

        //写入缓存
        cache.put(CachePrefix.ONLINE_MEMBER.getPrefix(), onlineMemberVOS);
    }

}
