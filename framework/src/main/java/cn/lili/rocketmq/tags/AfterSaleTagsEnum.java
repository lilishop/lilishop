package cn.lili.rocketmq.tags;

/**
 * 售后标签枚举
 *
 * @author paulG
 * @since 2020/12/9
 **/
public enum AfterSaleTagsEnum {

    /**
     * "售后退款"
     */
    REFUND("售后退款"),
    /**
     * "售后单状态改变"
     */
    AFTER_SALE_STATUS_CHANGE("售后单状态改变");

    private final String description;

    AfterSaleTagsEnum(String description) {
        this.description = description;
    }

    public String description() {
        return description;
    }


}
