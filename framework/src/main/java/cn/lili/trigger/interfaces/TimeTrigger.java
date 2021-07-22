package cn.lili.trigger.interfaces;

import cn.lili.trigger.model.TimeTriggerMsg;

/**
 * 延时执行接口
 *
 * @author Chopper
 */
public interface TimeTrigger {

    /**
     * 添加延时任务
     *
     * @param timeTriggerMsg 延时任务信息
     */
    void addDelay(TimeTriggerMsg timeTriggerMsg);


    /**
     * 执行延时任务
     *
     * @param timeTriggerMsg 延时任务信息
     */
    void execute(TimeTriggerMsg timeTriggerMsg);

    /**
     * 修改延时任务
     *
     * @param executorName   执行器beanId
     * @param param          执行参数
     * @param triggerTime    执行时间 时间戳 秒为单位
     * @param oldTriggerTime 旧的任务执行时间
     * @param uniqueKey      添加任务时的唯一凭证
     * @param delayTime      延时时间（秒）
     * @param topic          rocketmq topic
     */
    void edit(String executorName, Object param, Long oldTriggerTime, Long triggerTime, String uniqueKey, int delayTime, String topic);

    /**
     * 删除延时任务
     *
     * @param executorName 执行器
     * @param triggerTime  执行时间
     * @param uniqueKey    添加任务时的唯一凭证
     * @param topic        rocketmq topic
     */
    void delete(String executorName, Long triggerTime, String uniqueKey, String topic);
}
