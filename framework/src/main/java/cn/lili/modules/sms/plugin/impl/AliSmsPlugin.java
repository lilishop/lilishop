package cn.lili.modules.sms.plugin.impl;

import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.Base64Utils;
import cn.lili.modules.sms.entity.dos.SmsSign;
import cn.lili.modules.sms.entity.dos.SmsTemplate;
import cn.lili.modules.sms.entity.enums.SmsEnum;
import cn.lili.modules.sms.plugin.SmsPlugin;
import cn.lili.modules.system.entity.dto.SmsSetting;
import com.aliyun.dysmsapi20170525.models.*;
import com.aliyun.teaopenapi.models.Config;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
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
                .setTemplateParam(JSONUtil.toJsonStr(param));
        try {
            SendSmsResponse response = client.sendSms(sendSmsRequest);
            if (!("OK").equals(response.getBody().getCode())) {
                throw new ServiceException(response.getBody().getMessage());
            }
        } catch (Exception e) {
            log.error("发送短信错误", e);
        }
    }


    @Override
    public void sendBatchSms(String signName, List<String> mobile, String templateCode) {

        com.aliyun.dysmsapi20170525.Client client = this.createClient();

        List<String> sign = new ArrayList<String>();

        sign.addAll(mobile);
        sign.replaceAll(e -> signName);

        //手机号拆成多个小组进行发送
        List<List<String>> mobileList = new ArrayList<>();

        //签名名称多个小组
        List<List<String>> signNameList = new ArrayList<>();

        //循环分组
        for (int i = 0; i < (mobile.size() / 100 + (mobile.size() % 100 == 0 ? 0 : 1)); i++) {
            int endPoint = Math.min((100 + (i * 100)), mobile.size());
            mobileList.add(mobile.subList((i * 100), endPoint));
            signNameList.add(sign.subList((i * 100), endPoint));
        }

//       //发送短信
        for (int i = 0; i < mobileList.size(); i++) {
            SendBatchSmsRequest sendBatchSmsRequest = new SendBatchSmsRequest()
                    .setPhoneNumberJson(JSONUtil.toJsonStr(mobileList.get(i)))
                    .setSignNameJson(JSONUtil.toJsonStr(signNameList.get(i)))
                    .setTemplateCode(templateCode);
            try {
                client.sendBatchSms(sendBatchSmsRequest);
            } catch (Exception e) {
                log.error("批量发送短信错误", e);
            }
        }

    }

    @Override
    public void addSmsSign(SmsSign smsSign) throws Exception {
        //设置参数添加短信签名
        com.aliyun.dysmsapi20170525.Client client = this.createClient();
        //营业执照
        AddSmsSignRequest.AddSmsSignRequestSignFileList signFileList0 = new AddSmsSignRequest.AddSmsSignRequestSignFileList()
                .setFileContents(Base64Utils.encode(smsSign.getBusinessLicense()))
                .setFileSuffix(smsSign.getBusinessLicense().substring(smsSign.getBusinessLicense().lastIndexOf(".") + 1));
        //授权委托书
        AddSmsSignRequest.AddSmsSignRequestSignFileList signFileList1 = new AddSmsSignRequest.AddSmsSignRequestSignFileList()
                .setFileContents(Base64Utils.encode(smsSign.getLicense()))
                .setFileSuffix(smsSign.getLicense().substring(smsSign.getLicense().lastIndexOf(".")) + 1);
        //添加短信签名
        AddSmsSignRequest addSmsSignRequest = new AddSmsSignRequest()
                .setSignName(smsSign.getSignName())
                .setSignSource(smsSign.getSignSource())
                .setRemark(smsSign.getRemark())
                .setSignFileList(java.util.Arrays.asList(
                        signFileList0,
                        signFileList1
                ));
        AddSmsSignResponse response = client.addSmsSign(addSmsSignRequest);
        if (!("OK").equals(response.getBody().getCode())) {
            throw new ServiceException(response.getBody().getMessage());
        }
    }

    @Override
    public void deleteSmsSign(String signName) throws Exception {
        com.aliyun.dysmsapi20170525.Client client = this.createClient();
        DeleteSmsSignRequest deleteSmsSignRequest = new DeleteSmsSignRequest()
                .setSignName(signName);

        DeleteSmsSignResponse response = client.deleteSmsSign(deleteSmsSignRequest);
        if (!("OK").equals(response.getBody().getCode())) {
            throw new ServiceException(response.getBody().getMessage());
        }

    }

    @Override
    public Map<String, Object> querySmsSign(String signName) throws Exception {
        //设置参数查看短信签名
        com.aliyun.dysmsapi20170525.Client client = this.createClient();
        QuerySmsSignRequest querySmsSignRequest = new QuerySmsSignRequest().setSignName(signName);

        QuerySmsSignResponse response = client.querySmsSign(querySmsSignRequest);
        if (!("OK").equals(response.getBody().getCode())) {
            throw new ServiceException(response.getBody().getMessage());
        }
        Map<String, Object> map = new HashMap<>(2);
        map.put("SignStatus", response.getBody().getSignStatus());
        map.put("Reason", response.getBody().getReason());
        return map;
    }

    @Override
    public void modifySmsSign(SmsSign smsSign) throws Exception {
        //设置参数添加短信签名
        com.aliyun.dysmsapi20170525.Client client = this.createClient();

        ModifySmsSignRequest.ModifySmsSignRequestSignFileList signFileList0 = new ModifySmsSignRequest.ModifySmsSignRequestSignFileList()
                .setFileContents(Base64Utils.encode(smsSign.getBusinessLicense()))
                .setFileSuffix(smsSign.getBusinessLicense().substring(smsSign.getBusinessLicense().lastIndexOf(".") + 1));
        ModifySmsSignRequest.ModifySmsSignRequestSignFileList signFileList1 = new ModifySmsSignRequest.ModifySmsSignRequestSignFileList()
                .setFileContents(Base64Utils.encode(smsSign.getLicense()))
                .setFileSuffix(smsSign.getLicense().substring(smsSign.getBusinessLicense().lastIndexOf(".") + 1));
        ModifySmsSignRequest modifySmsSign = new ModifySmsSignRequest()
                .setSignName(smsSign.getSignName())
                .setSignSource(smsSign.getSignSource())
                .setRemark(smsSign.getRemark())
                .setSignFileList(java.util.Arrays.asList(
                        signFileList0,
                        signFileList1
                ));
        ModifySmsSignResponse response = client.modifySmsSign(modifySmsSign);
        if (!("OK").equals(response.getBody().getCode())) {
            throw new ServiceException(response.getBody().getMessage());
        }
    }

    @Override
    public void modifySmsTemplate(SmsTemplate smsTemplate) throws Exception {
        com.aliyun.dysmsapi20170525.Client client = this.createClient();
        ModifySmsTemplateRequest modifySmsTemplateRequest = new ModifySmsTemplateRequest()
                .setTemplateType(smsTemplate.getTemplateType())
                .setTemplateName(smsTemplate.getTemplateName())
                .setTemplateContent(smsTemplate.getTemplateContent())
                .setRemark(smsTemplate.getRemark())
                .setTemplateCode(smsTemplate.getTemplateCode());

        ModifySmsTemplateResponse response = client.modifySmsTemplate(modifySmsTemplateRequest);
        if (!("OK").equals(response.getBody().getCode())) {
            throw new ServiceException(response.getBody().getMessage());
        }
    }

    @Override
    public Map<String, Object> querySmsTemplate(String templateCode) throws Exception {
        com.aliyun.dysmsapi20170525.Client client = this.createClient();
        QuerySmsTemplateRequest querySmsTemplateRequest = new QuerySmsTemplateRequest()
                .setTemplateCode(templateCode);
        QuerySmsTemplateResponse response = client.querySmsTemplate(querySmsTemplateRequest);

        if (!("OK").equals(response.getBody().getCode())) {
            throw new ServiceException(response.getBody().getMessage());
        }
        Map<String, Object> map = new HashMap<>(4);
        map.put("TemplateStatus", response.getBody().getTemplateStatus());
        map.put("Reason", response.getBody().getReason());
        map.put("TemplateCode", response.getBody().getTemplateCode());
        return map;
    }

    @Override
    public String addSmsTemplate(SmsTemplate smsTemplate) throws Exception {
        com.aliyun.dysmsapi20170525.Client client = this.createClient();
        AddSmsTemplateRequest addSmsTemplateRequest = new AddSmsTemplateRequest()
                .setTemplateType(1)
                .setTemplateName(smsTemplate.getTemplateName())
                .setTemplateContent(smsTemplate.getTemplateContent())
                .setRemark(smsTemplate.getRemark());

        AddSmsTemplateResponse response = client.addSmsTemplate(addSmsTemplateRequest);
        if (!("OK").equals(response.getBody().getCode())) {
            throw new ServiceException(response.getBody().getMessage());
        }
        return response.getBody().getTemplateCode();
    }

    @Override
    public void deleteSmsTemplate(String templateCode) throws Exception {
        com.aliyun.dysmsapi20170525.Client client = this.createClient();
        DeleteSmsTemplateRequest deleteSmsTemplateRequest = new DeleteSmsTemplateRequest()
                .setTemplateCode(templateCode);

        DeleteSmsTemplateResponse response = client.deleteSmsTemplate(deleteSmsTemplateRequest);
        if (!("OK").equals(response.getBody().getCode())) {
            throw new ServiceException(response.getBody().getMessage());
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
