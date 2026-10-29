package cn.lili.timetask.config;

import cn.lili.timetask.TimedTaskJobHandler;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;

/**
 * 原 Spring 内置批量轮询定时调度（已由 DynamicTaskManager 动态可配置调度替代）
 * 默认不启用；若需要兼容旧版轮询模式，可在配置文件中设置 lili.schedule.legacy.enabled=true
 */
@Configuration
@EnableScheduling
@ConditionalOnProperty(name = "lili.schedule.legacy.enabled", havingValue = "true")
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
