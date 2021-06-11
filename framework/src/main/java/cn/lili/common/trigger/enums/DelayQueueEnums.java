package cn.lili.common.trigger.enums;

/**
 * 队列枚举
 */
public enum DelayQueueEnums {


    /**
     * 促销任务队列
     */
    PROMOTION_QUEUE("促销任务队列");

    private String description;

    DelayQueueEnums(String description) {
        this.description = description;
    }
}
