package cn.lili.timetask.config;

import cn.lili.timetask.TimedTaskJobHandler;
import org.junit.jupiter.api.Test;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

class TimedTaskSchedulerTest {

    @Test
    void runEveryMinuteDelegatesToTimedTaskHandler() {
        TimedTaskJobHandler handler = mock(TimedTaskJobHandler.class);
        TimedTaskScheduler scheduler = new TimedTaskScheduler(handler);

        scheduler.runEveryMinute();

        verify(handler).everyMinuteExecute("local-schedule");
    }

    @Test
    void runEveryHourDelegatesToTimedTaskHandler() {
        TimedTaskJobHandler handler = mock(TimedTaskJobHandler.class);
        TimedTaskScheduler scheduler = new TimedTaskScheduler(handler);

        scheduler.runEveryHour();

        verify(handler).everyHourExecuteJobHandler("local-schedule");
    }

    @Test
    void runEveryDayDelegatesToTimedTaskHandler() {
        TimedTaskJobHandler handler = mock(TimedTaskJobHandler.class);
        TimedTaskScheduler scheduler = new TimedTaskScheduler(handler);

        scheduler.runEveryDay();

        verify(handler).everyDayExecuteJobHandler("local-schedule");
    }
}
