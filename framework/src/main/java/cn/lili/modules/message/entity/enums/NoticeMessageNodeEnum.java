package cn.lili.modules.message.entity.enums;

/**
 * 消息编码枚举
 *
 * @author pikachu
 * @since 2020/12/8 9:46
 */
public enum NoticeMessageNodeEnum {

    /**
     * 订单 (会员)
     */
    /**
     * @deprecated 已废弃移除
     */
    @Deprecated
    ORDER_CREATE_SUCCESS("订单提交成功通知"),
    /**
     * @deprecated 已废弃移除
     */
    @Deprecated
    ORDER_CANCEL_SUCCESS("订单取消成功通知"),
    /**
     * @deprecated 已废弃移除
     */
    @Deprecated
    ORDER_PAY_SUCCESS("订单支付成功通知"),
    ORDER_PAY_ERROR("支付失败自动退款通知"),
    ORDER_DELIVER("订单发货通知"),
    ORDER_COMPLETE("订单完成通知"),

    /**
     * @deprecated 已废弃移除
     */
    @Deprecated
    ORDER_EVALUATION("订单评价提醒"),

    /**
     * 售后 (会员)
     */
    AFTER_SALE_AUDIT("售后审核通知"),
    @Deprecated
    AFTER_SALE_ROG("退货物品收货通知"),

    /**
     * @deprecated 已合并为 AFTER_SALE_ROG
     */
    @Deprecated
    AFTER_SALE_ROG_PASS("退货物品签收通知"),
    /**
     * @deprecated 已合并为 AFTER_SALE_ROG
     */
    @Deprecated
    AFTER_SALE_ROG_REFUSE("退货物品拒收通知"),
    /**
     * @deprecated 已废弃移除
     */
    @Deprecated
    AFTER_SALE_COMPLETE("售后完成通知"),

    /**
     * @deprecated 已废弃移除
     */
    @Deprecated
    AFTER_SALE_CREATE_SUCCESS("售后提交成功通知"),

    /**
     * @deprecated 已合并为 AFTER_SALE_AUDIT
     */
    @Deprecated
    RETURN_GOODS_PASS("退货审核通过通知"),
    /**
     * @deprecated 已合并为 AFTER_SALE_AUDIT
     */
    @Deprecated
    RETURN_MONEY_PASS("退款审核通过通知"),
    /**
     * @deprecated 已合并为 AFTER_SALE_AUDIT
     */
    @Deprecated
    RETURN_GOODS_REFUSE("退货审核未通过通知"),
    /**
     * @deprecated 已合并为 AFTER_SALE_AUDIT
     */
    @Deprecated
    RETURN_MONEY_REFUSE("退款审核未通过通知"),

    /**
     * 拼团 (会员)
     */
    PINTUAN_CREATE("开团成功通知"),
    PINTUAN_ERROR("拼团失败通知"),
    PINTUAN_SUCCESS("拼团成功通知"),

    /**
     * 积分 (会员)
     */
    POINT_CHANGE("积分变更通知"),

    /**
     * 用户余额 (会员)
     */
    WALLET_CHANGE("余额账户变更通知"),
    /**
     * @deprecated 已废弃移除
     */
    @Deprecated
    WALLET_WITHDRAWAL_CREATE("余额提现申请提交成功通知"),
    /**
     * @deprecated 已废弃移除
     */
    @Deprecated
    WALLET_WITHDRAWAL_SUCCESS("余额提现成功通知"),
    WALLET_WITHDRAWAL_ERROR("余额提现申请失败通知"),
    WALLET_WITHDRAWAL_AUDIT_ERROR("提现申请审核通知"),
    WALLET_WITHDRAWAL_AUDIT_SUCCESS("余额提现申请通过通知"),

    /**
     * 预约服务 (会员)
     */
    APPOINTMENT_BOOKING_SUCCESS_MEMBER("预约成功通知"),
    APPOINTMENT_SERVICE_REFUNDED_MEMBER("预约服务退款通知"),
    APPOINTMENT_SERVICE_START_REMIND_MEMBER("预约服务即将开始提醒"),

    /**
     * 商户模板消息 (商户)
     */
    STORE_AUDIT_RESULT("店铺入驻结果"),
    STORE_ORDER_PAY_SUCCESS("订单付款成功通知"),
    STORE_AFTER_SALE_APPLY("用户售后申请通知"),

    /**
     * 平台消息模板 (平台)
     */
    PLATFORM_STORE_AUDIT_NOTIFY("新店铺入驻待审核通知");


    private final String description;

    NoticeMessageNodeEnum(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }

}
