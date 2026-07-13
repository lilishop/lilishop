package cn.lili.timetask.config;

import cn.lili.timetask.TimedTaskJobHandler;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;

/**
 * Spring 内置定时调度，替代 XXL-Job。
 */
@Configuration
@EnableScheduling
public class TimedTaskScheduler {

    private static final String LOCAL_PARAM = "local-schedule";

    private final TimedTaskJobHandler timedTaskJobHandler;

    public TimedTaskScheduler(TimedTaskJobHandler timedTaskJobHandler) {
        this.timedTaskJobHandler = timedTaskJobHandler;
    }

    @Scheduled(cron = "${lili.schedule.every-minute-cron:0 * * * * ?}")
    public void runEveryMinute() {
        timedTaskJobHandler.everyMinuteExecute(LOCAL_PARAM);
    }

    @Scheduled(cron = "${lili.schedule.every-hour-cron:0 0 * * * ?}")
    public void runEveryHour() {
        timedTaskJobHandler.everyHourExecuteJobHandler(LOCAL_PARAM);
    }

    @Scheduled(cron = "${lili.schedule.every-day-cron:0 0 0 * * ?}")
    public void runEveryDay() {
        timedTaskJobHandler.everyDayExecuteJobHandler(LOCAL_PARAM);
    }
}
