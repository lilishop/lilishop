package cn.lili.modules.order.order.entity.enums;

/**
 * 交易状态枚举
 *
 * @author Chopper
 * @since 2020/11/17 7:27 下午
 */
public enum TradeStatusEnum {

    /**
     * 交易状态
     */
    UNPAID("未付款"),
    PAID("已付款"),
    CANCELLED("已取消");

    private final String description;

    TradeStatusEnum(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }

    public String description() {
        return this.description;
    }


}
