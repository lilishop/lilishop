package cn.lili.modules.order.order.entity.enums;


/**
 * 流水类型枚举
 *
 * @author Chopper
 * @since 2020/11/17 7:25 下午
 */
public enum FlowTypeEnum {

    /**
     * 流水类型
     */
    PAY("支付"),
    REFUND("退款");

    private final String description;

    FlowTypeEnum(String description) {
        this.description = description;
    }

    public String description() {
        return description;
    }
}
