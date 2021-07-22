package cn.lili.trigger;

import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.trigger.model.TimeTriggerMsg;
import cn.lili.trigger.util.DelayQueueTools;
import cn.lili.common.utils.SpringContextUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.spring.annotation.RocketMQMessageListener;
import org.apache.rocketmq.spring.core.RocketMQListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 事件触发消费者
 *
 * @author paulG
 * @since 2020/11/17 7:19 下午
 */
@Component
@Slf4j
@RocketMQMessageListener(topic = "${lili.data.rocketmq.promotion-topic}", consumerGroup = "${lili.data.rocketmq.promotion-group}")
public class TimeTriggerConsumer implements RocketMQListener<TimeTriggerMsg> {
    @Autowired
    private Cache<Integer> cache;

    @Override
    public void onMessage(TimeTriggerMsg timeTriggerMsg) {
        try {
            String key = DelayQueueTools.generateKey(timeTriggerMsg.getTriggerExecutor(), timeTriggerMsg.getTriggerTime(), timeTriggerMsg.getUniqueKey());

            if (cache.get(key) == null) {
                log.info("执行器执行被取消：{} | 任务标识：{}", timeTriggerMsg.getTriggerExecutor(), timeTriggerMsg.getUniqueKey());
                return;
            }

            log.info("执行器执行：" + timeTriggerMsg.getTriggerExecutor());
            log.info("执行器参数：" + JSONUtil.toJsonStr(timeTriggerMsg.getParam()));

            cache.remove(key);

            TimeTriggerExecutor executor = (TimeTriggerExecutor) SpringContextUtil.getBean(timeTriggerMsg.getTriggerExecutor());
            executor.execute(timeTriggerMsg.getParam());
        } catch (Exception e) {
            log.error("mq延时任务异常", e);
        }

    }

}
