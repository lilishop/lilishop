package cn.lili.modules.message.entity.enums;

/**
 * 微信模版设置变量
 *
 * @author Chopper
 * @version v1.0
 * 2020-12-10 17:47
 */
public enum WechatMessageItemEnums {
    SHOP_NAME(new String[]{"商户名称"}),
    MEMBER_NAME(new String[]{"买家昵称"}),
    PRICE(new String[]{"支付金额","订单金额"}),

    GOODS_INFO(new String[]{"订单详情","商品清单","商品名称"}),

    ORDER_SN(new String[]{"订单编号"}),


    LOGISTICS_NAME(new String[]{"快递公司"}),
    LOGISTICS_NO(new String[]{"快递单号"}),

    LOGISTICS_TIME(new String[]{"发货时间"}),
    PAYMENT_TIME(new String[]{"支付时间"}),
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
