package cn.lili.common.trigger.interfaces;

import cn.lili.common.trigger.model.TimeTriggerMsg;

/**
 * 延时执行接口
 *
 * @author Chopper
 */
public interface TimeTrigger {

    /**
     * 添加延时任务
     *
     * @param executorName 执行器beanId
     * @param param        执行参数
     * @param triggerTime  执行时间 时间戳 秒为单位
     * @param uniqueKey    如果是一个 需要有 修改/取消 延时任务功能的延时任务，<br/>
     *                     请填写此参数，作为后续删除，修改做为唯一凭证 <br/>
     *                     建议参数为：COUPON_{ACTIVITY_ID} 例如 coupon_123<br/>
     *                     业务内全局唯一
     * @param topic        rocketmq topic
     */
    void add(String executorName, Object param, Long triggerTime, String uniqueKey, String topic);

    /**
     * 添加延时任务
     *
     * @param timeTriggerMsg 延时任务信息
     */
    void add(TimeTriggerMsg timeTriggerMsg);


    /**
     * 添加延时任务
     *
     * @param timeTriggerMsg 延时任务信息
     * @param delayTime      延时时间（秒）
     */
    void addDelay(TimeTriggerMsg timeTriggerMsg, int delayTime);

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
