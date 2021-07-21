package cn.lili.modules.order.trade.entity.enums;

/**
 * 售后类型
 *
 * @author Chopper
 * @since 2020/8/20 15:39
 */

public enum AfterSaleTypeEnum {
    /**
     * 售后服务类型枚举
     */
    RETURN_MONEY("退款"), RETURN_GOODS("退货");

    private final String description;

    AfterSaleTypeEnum(String description) {
        this.description = description;
    }

    public String description() {
        return description;
    }

}
