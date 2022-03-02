package cn.lili.common.properties;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

/**
 * 短信模版设置
 *
 * @author Chopper
 */
@Data
@Configuration
@ConfigurationProperties(prefix = "lili.sms")
public class SmsTemplateProperties {
    /**
     * 登录
     */
    private String lOGIN = "SMS_185821482";
    /**
     * 注册
     */
    private String REGISTER = "SMS_185821482";
    /**
     * 找回密码
     */
    private String FIND_USER = "SMS_185821482";
    /**
     * 设置密码
     */
    private String UPDATE_PASSWORD = "SMS_185821482";
    /**
     * 设置支付密码
     */
    private String WALLET_PASSWORD = "SMS_185821482";
}
