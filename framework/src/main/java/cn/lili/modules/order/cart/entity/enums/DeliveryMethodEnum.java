package cn.lili.modules.order.cart.entity.enums;

/**
 * 配送方式
 *
 * @author paulG
 * @date 2020-03-25 2:30 下午
 **/
public enum DeliveryMethodEnum {

    SELF_PICK_UP("自提"),
    LOCAL_TOWN_DELIVERY("同城配送"),
    LOGISTICS("物流");

    private final String description;

    DeliveryMethodEnum(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }

}
