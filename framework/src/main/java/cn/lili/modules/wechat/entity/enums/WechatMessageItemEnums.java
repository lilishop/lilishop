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
    GOODS_INFO(new String[]{"订单详情","商品清单","商品名称"}),
    /**
     * 订单编号
     */
    ORDER_SN(new String[]{"订单编号"}),
    /**
     * 快递公司
     */
    LOGISTICS_NAME(new String[]{"快递公司"}),
    /**
     * 快递单号
     */
    LOGISTICS_NO(new String[]{"快递单号"}),
    /**
     * 发货时间
     */
    LOGISTICS_TIME(new String[]{"发货时间"}),
    /**
     * 支付时间
     */
    PAYMENT_TIME(new String[]{"支付时间"})
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
