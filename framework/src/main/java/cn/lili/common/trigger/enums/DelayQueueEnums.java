package cn.lili.common.trigger.enums;

/**
 * 队列枚举
 *
 * @author Bulbasaur
 */
public enum DelayQueueEnums {


    /**
     * 促销活动
     */
    PROMOTION("促销活动");

    private String description;

    DelayQueueEnums(String description) {
        this.description = description;
    }
}
