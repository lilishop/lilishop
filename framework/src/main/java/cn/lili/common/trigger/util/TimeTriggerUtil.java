package cn.lili.common.trigger.util;

/**
 * 延时任务mq实现内容，提供加密算法以及任务前缀参数
 *
 * @author Chopper
 */
public class TimeTriggerUtil {

    /**
     * 前缀
     */
    private static final String PREFIX = "{rocketmq_trigger}_";

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
