package cn.lili.timetask.config;

import cn.lili.timetask.TimedTaskJobHandler;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;

/**
 * 精简模式没有 XXLJob Admin 时，用本地 Spring 定时器调用同一套任务处理器。
 */
@Configuration
@EnableScheduling
@ConditionalOnProperty(prefix = "xxl.job", name = "enabled", havingValue = "false")
public class LocalTimedTaskScheduler {

    private static final String LOCAL_PARAM = "local-schedule";

    private final TimedTaskJobHandler timedTaskJobHandler;

    public LocalTimedTaskScheduler(TimedTaskJobHandler timedTaskJobHandler) {
        this.timedTaskJobHandler = timedTaskJobHandler;
    }

    @Scheduled(cron = "${lili.lite.local-schedule.every-minute-cron:0 * * * * ?}")
    public void runEveryMinute() {
        timedTaskJobHandler.everyMinuteExecute(LOCAL_PARAM);
    }

    @Scheduled(cron = "${lili.lite.local-schedule.every-hour-cron:0 0 * * * ?}")
    public void runEveryHour() {
        timedTaskJobHandler.everyHourExecuteJobHandler(LOCAL_PARAM);
    }

    @Scheduled(cron = "${lili.lite.local-schedule.every-day-cron:0 0 0 * * ?}")
    public void runEveryDay() {
        timedTaskJobHandler.everyDayExecuteJobHandler(LOCAL_PARAM);
    }
}
