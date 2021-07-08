package cn.lili.common.trigger.enums;

/**
 * 队列枚举
 *
 * @author Bulbasaur
 * @date: 2021/7/9 1:40 上午
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
