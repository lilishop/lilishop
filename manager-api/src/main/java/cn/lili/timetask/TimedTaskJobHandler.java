package cn.lili.timetask;

import cn.lili.timetask.handler.EveryDayExecute;
import cn.lili.timetask.handler.EveryHourExecute;
import cn.lili.timetask.handler.EveryMinuteExecute;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * 定时器任务（由 {@link cn.lili.timetask.config.TimedTaskScheduler} 调度）
 *
 * @author Chopper
 */
@Slf4j
@Component
public class TimedTaskJobHandler {

    @Autowired
    private List<EveryMinuteExecute> everyMinuteExecutes;

    @Autowired
    private List<EveryHourExecute> everyHourExecutes;

    @Autowired
    private List<EveryDayExecute> everyDayExecutes;

    public void everyMinuteExecute(String param) {
        log.info("每分钟任务执行");
        if (everyMinuteExecutes == null || everyMinuteExecutes.isEmpty()) {
            return;
        }
        for (EveryMinuteExecute execute : everyMinuteExecutes) {
            try {
                execute.execute();
            } catch (Exception e) {
                log.error("每分钟任务异常", e);
            }
        }
    }

    public void everyHourExecuteJobHandler(String param) {
        log.info("每小时任务执行");
        if (everyHourExecutes == null || everyHourExecutes.isEmpty()) {
            return;
        }
        for (EveryHourExecute execute : everyHourExecutes) {
            try {
                execute.execute();
            } catch (Exception e) {
                log.error("每小时任务异常", e);
            }
        }
    }

    public void everyDayExecuteJobHandler(String param) {
        log.info("每日任务执行");
        if (everyDayExecutes == null || everyDayExecutes.isEmpty()) {
            return;
        }
        for (EveryDayExecute execute : everyDayExecutes) {
            try {
                execute.execute();
            } catch (Exception e) {
                log.error("每日任务异常", e);
            }
        }
    }
}
