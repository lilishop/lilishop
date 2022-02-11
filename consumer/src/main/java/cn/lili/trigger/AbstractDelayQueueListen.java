package cn.lili.trigger;

import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.common.utils.ThreadPoolUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.ApplicationRunner;
import org.springframework.data.redis.core.DefaultTypedTuple;
import org.springframework.util.CollectionUtils;

import java.util.Set;
import java.util.concurrent.TimeUnit;

/**
 * 延时队列工厂
 *
 * @author paulG
 * @since 2020/11/7
 **/
@Slf4j
public abstract class AbstractDelayQueueListen implements ApplicationRunner {

    @Autowired
    private Cache cache;


    /**
     * 延时队列机器开始运作
     */
    private void startDelayQueueMachine() {
        log.info("延时队列机器{}开始运作", setDelayQueueName());

        //监听redis队列
        while (true) {
            try {
                //获取当前时间的时间戳
                long now = System.currentTimeMillis() / 1000;
                //获取当前时间前需要执行的任务列表
                Set<DefaultTypedTuple> tuples = cache.zRangeByScore(setDelayQueueName(), 0, now);

                //如果任务不为空
                if (!CollectionUtils.isEmpty(tuples)) {
                    log.info("执行任务:{}", JSONUtil.toJsonStr(tuples));

                    for (DefaultTypedTuple tuple : tuples) {
                        String jobId = (String) tuple.getValue();
                        //移除缓存，如果移除成功则表示当前线程处理了延时任务，则执行延时任务
                        Long num = cache.zRemove(setDelayQueueName(), jobId);
                        //如果移除成功, 则执行
                        if (num > 0) {
                            ThreadPoolUtil.execute(() -> invoke(jobId));
                        }
                    }
                }

            } catch (Exception e) {
                log.error("处理延时任务发生异常,异常原因为{}", e.getMessage(), e);
            } finally {
                //间隔一秒钟搞一次
                try {
                    TimeUnit.SECONDS.sleep(5L);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }

            }
        }

    }

    /**
     * 最终执行的任务方法
     *
     * @param jobId 任务id
     */
    public abstract void invoke(String jobId);


    /**
     * 要实现延时队列的名字
     * @return 促销延时队列名称
     */
    public abstract String setDelayQueueName();


    /**
     * 监听队列
     */
    public void init() {
        ThreadPoolUtil.getPool().execute(this::startDelayQueueMachine);
    }

}
