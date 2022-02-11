package cn.lili.rocketmq.tags;

/**
 * 订单操作枚举
 *
 * @author paulG
 * @since 2020/12/9
 **/
public enum OrderTagsEnum {

    /**
     * 订单创建
     */
    ORDER_CREATE("订单创建"),
    /**
     * 订单状态改变
     */
    STATUS_CHANGE("订单状态改变");


    private final String description;

    OrderTagsEnum(String description) {
        this.description = description;
    }

    public String description() {
        return description;
    }


}
