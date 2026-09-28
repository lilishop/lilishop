package cn.lili.modules.message.entity.enums;

import cn.hutool.core.text.CharSequenceUtil;

/**
 * 消息编码枚举
 *
 * @author pikachu
 * @since 2020/12/8 9:46
 */
public enum NoticeMessageNodeEnum {

    /**
     * 订单
     */
    ORDER_CREATE_SUCCESS("订单提交成功通知"),
    ORDER_CANCEL_SUCCESS("订单取消成功通知"),
    ORDER_PAY_SUCCESS("订单支付成功通知"),
    ORDER_PAY_ERROR("支付失败自动退款通知"),
    ORDER_DELIVER("订单发货通知"),
    ORDER_COMPLETE("订单完成通知"),
    ORDER_EVALUATION("订单评价提醒"),

    /**
     * 售后
     */
    AFTER_SALE_CREATE_SUCCESS("售后提交成功通知"),
    RETURN_GOODS_PASS("退货审核通过通知"),
    RETURN_MONEY_PASS("退款审核通过通知"),
    RETURN_GOODS_REFUSE("退货审核未通过通知"),
    RETURN_MONEY_REFUSE("退款审核未通过通知"),
    AFTER_SALE_ROG_PASS("退货物品签收通知"),
    AFTER_SALE_ROG_REFUSE("退货物品拒收通知"),
    AFTER_SALE_COMPLETE("售后完成通知"),


    /**
     * 拼团
     */
    PINTUAN_CREATE("开团成功通知"),
    PINTUAN_ERROR("拼团失败通知"),
    PINTUAN_SUCCESS("拼团成功通知"),

    /**
     * 积分
     */
    POINT_CHANGE("积分变更通知"),

    /**
     * 用户余额
     */
    WALLET_CHANGE("余额账户变更通知"),
    WALLET_WITHDRAWAL_CREATE("余额提现申请提交成功通知"),
    WALLET_WITHDRAWAL_SUCCESS("余额提现成功通知"),
    WALLET_WITHDRAWAL_ERROR("余额提现申请失败通知"),
    WALLET_WITHDRAWAL_AUDIT_ERROR("余额提现申请驳回通知"),
    WALLET_WITHDRAWAL_AUDIT_SUCCESS("余额提现申请通过通知"),

    /**
     * 预约服务
     */
    APPOINTMENT_BOOKING_SUCCESS_MEMBER("预约成功通知"),
    APPOINTMENT_SERVICE_REFUNDED_MEMBER("预约服务退款通知"),
    APPOINTMENT_SERVICE_START_REMIND_MEMBER("预约服务即将开始提醒");


    private final String description;

    NoticeMessageNodeEnum(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }

    /**
     * 将库中 {@code notice_node} 文案解析为枚举名，供 {@code scene_code} 与微信模板关联。
     */
    public static NoticeMessageNodeEnum fromNoticeNodeLabel(String noticeNode) {
        if (CharSequenceUtil.isBlank(noticeNode)) {
            return null;
        }
        String t = noticeNode.replace("\r", "").replace("\n", "").trim();
        for (NoticeMessageNodeEnum e : values()) {
            if (e.getDescription().equals(t)) {
                return e;
            }
        }
        return null;
    }

}
