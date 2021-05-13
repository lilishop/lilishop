package cn.lili.common.rocketmq.tags;

/**
 * @author paulG
 * @since 2020/12/9
 **/
public enum AfterSaleTagsEnum {

    REFUND("售后退款"),
    AFTER_SALE_STATUS_CHANGE("售后单状态改变");

    private final String description;

    AfterSaleTagsEnum(String description) {
        this.description = description;
    }

    public String description() {
        return description;
    }


}
