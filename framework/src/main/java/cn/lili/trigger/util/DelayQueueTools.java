package cn.lili.trigger.util;

import cn.lili.trigger.enums.DelayTypeEnums;

/**
 * 延时任务工具类
 *
 * @author paulG
 * @since 2021/5/7
 **/
public class DelayQueueTools {

    /**
     * 前缀
     */
    private static final String PREFIX = "{rocketmq_trigger}_";

    /**
     * 组装延时任务唯一键
     *
     * @param type 延时任务类型
     * @param id   id
     * @return 唯一键
     */
    public static String wrapperUniqueKey(DelayTypeEnums type, String id) {
        return "{TIME_TRIGGER_" + type.name() + "}_" + id;
    }


    /**
     * 生成延时任务标识key
     *
     * @param executorName 执行器beanId
     * @param triggerTime  执行时间
     * @param uniqueKey    自定义表示
     * @return 延时任务标识key
     */
    public static String generateKey(String executorName, Long triggerTime, String uniqueKey) {
        return PREFIX + (executorName + triggerTime + uniqueKey).hashCode();
    }

}
