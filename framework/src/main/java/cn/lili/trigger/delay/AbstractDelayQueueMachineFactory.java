package cn.lili.trigger.delay;

import cn.lili.cache.Cache;
import cn.lili.common.utils.DateUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * 延时队列工厂
 *
 * @author paulG
 * @since 2020/11/7
 **/
@Slf4j
public abstract class AbstractDelayQueueMachineFactory {

    @Autowired
    private Cache cache;

    /**
     * 插入任务id
     *
     * @param jobId       任务id(队列内唯一)
     * @param triggerTime 执行时间 时间戳（毫秒）
     * @return 是否插入成功
     */
    public boolean addJob(String jobId, Long triggerTime) {

        //redis 中排序时间
        long delaySeconds = triggerTime / 1000;
        //增加延时任务 参数依次为：队列名称、执行时间、任务id
        boolean result = cache.zAdd(setDelayQueueName(), delaySeconds, jobId);
        log.info("增加延时任务, 缓存key {}, 执行时间 {},任务id {}", setDelayQueueName(), DateUtil.toString(triggerTime), jobId);
        return result;

    }


    /**
     * 要实现延时队列的名字
     * @return 延时队列的名字
     */
    public abstract String setDelayQueueName();

}
