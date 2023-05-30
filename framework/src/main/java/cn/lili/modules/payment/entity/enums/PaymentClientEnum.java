package cn.lili.modules.payment.entity.enums;

/**
 * 支付方式枚举
 *
 * @author Chopper
 * @since 2020/12/18 18:08
 */
public enum PaymentClientEnum {

    /**
     * app支付
     **/
    APP,
    /**
     * 展示二维码扫描支付
     */
    NATIVE,
    /**
     * 公众号内部调用支付
     */
    JSAPI,
    /**
     * 普通移动网页调用支付app
     */
    H5,
    /**
     *  小程序，通常指微信小程序
     */
    MP
}
