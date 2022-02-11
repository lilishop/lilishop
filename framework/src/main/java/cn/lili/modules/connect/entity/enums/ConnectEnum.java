package cn.lili.modules.connect.entity.enums;

/**
 * 联合登陆枚举
 *
 * @author Chopper
 * @version v4.0
 * @since 2020/11/25 18:20
 */
public enum ConnectEnum {
    /**
     * 联合登陆枚举各个类型
     */
    QQ("QQ登录"),
    WEIBO("微博联合登录"),
    //只存放unionid
    WECHAT("微信联合登录"),
    WECHAT_OPEN_ID("微信openid登录"),
    WECHAT_MP_OPEN_ID("微信openid登录"),
    ALIPAY("支付宝登录"),
    APPLE("苹果登录");

    private final String description;

    ConnectEnum(String description) {
        this.description = description;
    }

}
