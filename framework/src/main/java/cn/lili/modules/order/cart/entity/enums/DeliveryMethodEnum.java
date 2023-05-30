package cn.lili.modules.order.cart.entity.enums;

/**
 * 配送方式
 *
 * @author paulG
 * @since 2020-03-25 2:30 下午
 **/
public enum DeliveryMethodEnum {

    /**
     * "自提"
     */
    SELF_PICK_UP("自提"),
    /**
     * "同城配送"
     */
    LOCAL_TOWN_DELIVERY("同城配送"),
    /**
     * "物流"
     */
    LOGISTICS("物流");

    private final String description;

    DeliveryMethodEnum(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }

}
