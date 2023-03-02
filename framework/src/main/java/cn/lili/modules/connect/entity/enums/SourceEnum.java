package cn.lili.modules.connect.entity.enums;

/**
 * 联合登陆-渠道枚举
 *
 * @author Chopper
 * @version v4.0
 * @since 2020/11/25 18:20
 */
public enum SourceEnum {

    WECHAT_PC_OPEN_ID("微信PC应用 openid登录"),
    WECHAT_OFFIACCOUNT_OPEN_ID("微信公众号 openid登录"),
    WECHAT_MP_OPEN_ID("微信小程序 openid登录"),
    WECHAT_APP_OPEN_ID("微信APP openid登录"),
    ;


    private final String description;

    SourceEnum(String description) {
        this.description = description;
    }
}
