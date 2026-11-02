package cn.lili.modules.wechat.entity.enums;

/**
 * 微信模版设置变量
 *
 * @author Chopper
 * @version v1.0
 * 2020-12-10 17:47
 */
public enum WechatMessageItemEnums {
    /**
     * 商户名称
     */
    SHOP_NAME(new String[]{"商户名称"}),
    /**
     * 买家昵称
     */
    MEMBER_NAME(new String[]{"买家昵称"}),
    /**
     * 订单金额
     */
    PRICE(new String[]{"支付金额","订单金额"}),
    /**
     * 订单详情
     */
    GOODS_INFO(new String[]{"订单详情","商品清单","商品名称","物品名称"}),
    /**
     * 订单编号
     */
    ORDER_SN(new String[]{"订单编号","订单号"}),
    /**
     * 快递公司
     */
    LOGISTICS_NAME(new String[]{"快递公司","物流公司"}),
    /**
     * 快递单号
     */
    LOGISTICS_NO(new String[]{"快递单号","物流单号"}),
    /**
     * 发货时间
     */
    LOGISTICS_TIME(new String[]{"发货时间"}),
    /**
     * 支付时间
     */
    PAYMENT_TIME(new String[]{"支付时间"}),
    /**
     * 温馨提示 / 备注
     */
    COMPLETE_TIME(new String[]{"完成时间"}),
    /**
     * 成团时间
     */
    PINTUAN_TIME(new String[]{"成团时间"}),
    /**
     * 售后单号
     */
    AFTER_SALE_SN(new String[]{"售后单号"}),
    /**
     * 审核结果
     */
    AUDIT_RESULT(new String[]{"审核结果"}),
    /**
     * 变动积分数量
     */
    CHANGE_POINT(new String[]{"变动数量"}),
    /**
     * 当前积分
     */
    CURRENT_POINT(new String[]{"当前积分","剩余积分"}),
    /**
     * 积分变更时间
     */
    POINT_TIME(new String[]{"积分变更时间","时间"}),
    /**
     * 变动金额
     */
    CHANGE_AMOUNT(new String[]{"变动金额"}),
    /**
     * 账户余额
     */
    WALLET_BALANCE(new String[]{"账户余额"}),
    /**
     * 变动时间
     */
    WALLET_TIME(new String[]{"时间"}),
    /**
     * 变动原因
     */
    CHANGE_REASON(new String[]{"变动原因"}),
    /**
     * 提现金额
     */
    WITHDRAW_AMOUNT(new String[]{"提现金额"}),
    /**
     * 提现结果
     */
    WITHDRAW_RESULT(new String[]{"提现结果"}),
    /**
     * 申请时间
     */
    APPLY_TIME(new String[]{"申请时间"}),
    /**
     * 温馨提醒
     */
    WARM_REMIND(new String[]{"温馨提醒"}),
    /**
     * 温馨提示 / 备注
     */
    REMARK(new String[]{"温馨提示","备注"})
    ;

    /**
     * 名称
     */
    private String[] text;

    WechatMessageItemEnums(String[] text) {
        this.text = text;
    }

    public String[] getText() {
        return text;
    }
}
