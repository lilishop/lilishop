package cn.lili.modules.sms.plugin;

import cn.lili.modules.sms.entity.enums.SmsEnum;
import java.util.Map;

/**
 * 短信发送插件。签名和模板在短信服务商控制台维护。
 *
 * @author Bulbasaur
 * @since 2023-02-16
 */
public interface SmsPlugin {
    /** 返回短信服务商类型。 */
    SmsEnum pluginName();

    /** 使用已配置的签名和模板发送验证码或业务通知。 */
    void sendSmsCode(String signName, String mobile, Map<String, String> param, String templateCode);
}
