package cn.lili.common.delayqueue;

/**
 * 延时任务类型
 *
 * @author paulG
 * @since 2021/5/7
 **/
public enum DelayQueueType {

    /**
     * 促销活动
     */
    PROMOTION("促销活动"),
    /**
     * 拼团订单
     */
    PINTUAN_ORDER("拼团订单"),

    /**
     * 直播
     */
    BROADCAST("直播");

    private final String description;

    DelayQueueType(String des) {
        this.description = des;
    }

    public String description() {
        return this.description;
    }

}
