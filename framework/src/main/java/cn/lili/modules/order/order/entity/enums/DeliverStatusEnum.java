package cn.lili.modules.order.order.entity.enums;

/**
 * 发货状态枚举
 *
 * @author Chopper
 * @since 2020/11/17 7:25 下午
 */
public enum DeliverStatusEnum {

    /**
     * 发货状态
     */
    UNDELIVERED("未发货"),
    DELIVERED("已发货"),
    RECEIVED("已收货");


    private final String description;

    DeliverStatusEnum(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }

}
