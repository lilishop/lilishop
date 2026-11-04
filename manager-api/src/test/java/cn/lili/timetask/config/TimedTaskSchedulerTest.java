package cn.lili.timetask.config;

import cn.lili.timetask.TimedTaskJobHandler;
import org.junit.jupiter.api.Test;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

class TimedTaskSchedulerTest {

    /**
     * 验证每分钟调度入口委托对应定时任务处理器。
     */
    @Test
    void runEveryMinuteDelegatesToTimedTaskHandler() {
        TimedTaskJobHandler handler = mock(TimedTaskJobHandler.class);
        TimedTaskScheduler scheduler = new TimedTaskScheduler(handler);

        scheduler.runEveryMinute();

        verify(handler).everyMinuteExecute("local-schedule");
    }

    /**
     * 验证每小时调度入口委托对应定时任务处理器。
     */
    @Test
    void runEveryHourDelegatesToTimedTaskHandler() {
        TimedTaskJobHandler handler = mock(TimedTaskJobHandler.class);
        TimedTaskScheduler scheduler = new TimedTaskScheduler(handler);

        scheduler.runEveryHour();

        verify(handler).everyHourExecuteJobHandler("local-schedule");
    }

    /**
     * 验证每天调度入口委托对应定时任务处理器。
     */
    @Test
    void runEveryDayDelegatesToTimedTaskHandler() {
        TimedTaskJobHandler handler = mock(TimedTaskJobHandler.class);
        TimedTaskScheduler scheduler = new TimedTaskScheduler(handler);

        scheduler.runEveryDay();

        verify(handler).everyDayExecuteJobHandler("local-schedule");
    }
}
