package cn.lili.modules.order.trade.entity.enums;

/**
 * 退款方式
 *
 * @author paulG
 * @since 2020/12/4
 **/
public enum AfterSaleRefundWayEnum {


    ORIGINAL("原路退回"),
    OFFLINE("线下支付");

    private final String description;

    AfterSaleRefundWayEnum(String description) {
        this.description = description;
    }

    public String description() {
        return description;
    }

}
