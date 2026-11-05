package cn.lili.modules.sms.plugin.impl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.sms.entity.enums.SmsEnum;
import cn.lili.modules.sms.plugin.SmsPlugin;
import cn.lili.modules.system.entity.dto.SmsSetting;
import com.alibaba.fastjson2.JSON;
import com.aliyun.dysmsapi20170525.models.SendSmsRequest;
import com.aliyun.dysmsapi20170525.models.SendSmsResponse;
import com.aliyun.teaopenapi.models.Config;
import lombok.extern.slf4j.Slf4j;

import java.util.Map;

/**
 * 阿里云短信插件
 *
 * @author Bulbasaur
 * @since 2023-02-16
 */
@Slf4j
public class AliSmsPlugin implements SmsPlugin {

    private SmsSetting smsSetting;

    public AliSmsPlugin(SmsSetting smsSetting) {
        this.smsSetting = smsSetting;
    }

    @Override
    public SmsEnum pluginName() {
        return SmsEnum.ALI;
    }

    @Override
    public void sendSmsCode(String signName, String mobile, Map<String, String> param, String templateCode) {
        com.aliyun.dysmsapi20170525.Client client = this.createClient();
        SendSmsRequest sendSmsRequest = new SendSmsRequest()
                .setSignName(signName)
                .setPhoneNumbers(mobile)
                .setTemplateCode(templateCode)
                .setTemplateParam(JSON.toJSONString(param));
        try {
            SendSmsResponse response = client.sendSms(sendSmsRequest);
            if (!("OK").equals(response.getBody().getCode())) {
                throw new ServiceException(response.getBody().getMessage());
            }
        } catch (Exception e) {
            log.error("发送短信错误", e);
        }
    }



    /**
     * 初始化账号Client
     *
     * @return Client 短信操作
     */
    public com.aliyun.dysmsapi20170525.Client createClient() {
        try {
            if (smsSetting == null) {
                throw new ServiceException(ResultCode.ALI_SMS_SETTING_ERROR);
            }
            Config config = new Config();
            //您的AccessKey ID
            config.accessKeyId = smsSetting.getAccessKeyId();
            //您的AccessKey Secret
            config.accessKeySecret = smsSetting.getAccessSecret();
            //访问的域名
            config.endpoint = "dysmsapi.aliyuncs.com";
            return new com.aliyun.dysmsapi20170525.Client(config);
        } catch (Exception e) {
            log.error("短信初始化错误", e);
        }
        return null;
    }

}
