package cn.lili.common.delayqueue;

/**
 * 延时任务工具类
 *
 * @author paulG
 * @since 2021/5/7
 **/
public class DelayQueueTools {

    /**
     * 组装延时任务唯一键
     *
     * @param type 延时任务类型
     * @param id   id
     * @return 唯一键
     */
    public static String wrapperUniqueKey(DelayQueueType type, String id) {
        return "{TIME_TRIGGER_" + type.name() + "}_" + id;
    }

}
