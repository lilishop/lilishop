package cn.lili.timetask;

import cn.lili.timetask.handler.EveryDayExecute;
import cn.lili.timetask.handler.EveryHourExecute;
import cn.lili.timetask.handler.EveryMinuteExecute;
import com.xxl.job.core.biz.model.ReturnT;
import com.xxl.job.core.handler.annotation.XxlJob;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * 定时器任务
 *
 * @author Chopper
 * @version v1.0
 * 2020-12-24 11:51
 */
@Slf4j
@Component
public class TimedTaskJobHandler {

    @Autowired(required = false)
    private List<EveryMinuteExecute> everyMinuteExecutes;


    @Autowired(required = false)
    private List<EveryHourExecute> everyHourExecutes;


    @Autowired(required = false)
    private List<EveryDayExecute> everyDayExecutes;

    /**
     * 每分钟任务
     *
     * @throws Exception
     */
    @XxlJob("everyMinuteExecute")
    public ReturnT<String> everyMinuteExecute(String param)  {
        log.info("每分钟任务执行");
        if (everyMinuteExecutes == null || everyMinuteExecutes.size() == 0) {
            return ReturnT.SUCCESS;
        }

        for (int i = 0; i < everyMinuteExecutes.size(); i++) {
            try {
                everyMinuteExecutes.get(i).execute();
            } catch (Exception e) {
                log.error("每分钟任务异常", e);
            }
        }
        return ReturnT.SUCCESS;
    }

    /**
     * 每小时任务
     *
     * @throws Exception
     */
    @XxlJob("everyHourExecuteJobHandler")
    public ReturnT<String> everyHourExecuteJobHandler(String param) {
        log.info("每小时任务执行");
        if (everyHourExecutes == null || everyHourExecutes.size() == 0) {
            return ReturnT.SUCCESS;
        }

        for (int i = 0; i < everyHourExecutes.size(); i++) {
            try {
                everyHourExecutes.get(i).execute();
            } catch (Exception e) {
                log.error("每小时任务异常", e);
            }
        }
        return ReturnT.SUCCESS;
    }

    /**
     * 每日任务
     *
     * @throws Exception
     */
    @XxlJob("everyDayExecuteJobHandler")
    public ReturnT<String> everyDayExecuteJobHandler(String param) {

        log.info("每日任务执行");
        if (everyDayExecutes == null || everyDayExecutes.size() == 0) {
            return ReturnT.SUCCESS;
        }

        for (int i = 0; i < everyDayExecutes.size(); i++) {
            try {
                everyDayExecutes.get(i).execute();
            } catch (Exception e) {
                log.error("每日任务异常", e);
            }
        }
        return ReturnT.SUCCESS;
    }


}
