package cn.lili.modules.order.trade.entity.enums;

/**
 * 售后状态
 *
 * @author Chopper
 * @since 2020-08-20 15:39
 */
public enum AfterSaleStatusEnum {

    /**
     * 售后服务类型枚举
     */
    APPLY("申请中"),
    PASS("已通过"),
    REFUSE("已拒绝"),
    BUYER_RETURN("待卖家收货"),
    SELLER_CONFIRM("卖家确认收货"),
    SELLER_TERMINATION("卖家终止售后"),
    BUYER_CANCEL("买家取消售后"),
    WAIT_REFUND("等待平台退款"),
    COMPLETE("完成");

    private final String description;

    AfterSaleStatusEnum(String description) {
        this.description = description;
    }

    public String description() {
        return description;
    }


}
