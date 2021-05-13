package cn.lili.modules.connect.entity.enums;

/**
 * 联合登录配置
 *
 * @author Chopper
 * @version v1.0
 * 2020-11-25 18:23
 */
public enum ConnectConfigEnum {

    /**
     * 微信网页
     * 微信小程序
     * 微信APP
     * 支付宝
     * 微博
     * qq
     */
    WEIXIN_WEB("微信网页配置", "app_key,app_secret"),
    WEIXIN_MP("微信小程序配置", "app_key,app_secret"),
    WEIXIN_APP("微信APP配置", "app_key,app_secret"),
    ALIPAY("支付宝配置", "app_id,private_key,public_key"),
    QQ("QQ配置", "app_id,app_key"),
    WEIBO("微博配置", "app_key,app_secret"),
    ;

    /**
     * 名称
     */
    String name;

    /**
     * 表单项
     */
    String form;

    ConnectConfigEnum(String name, String form) {
        this.name = name;
        this.form = form;
    }

    public String getName() {
        return name;
    }

    public String getForm() {
        return form;
    }
}
