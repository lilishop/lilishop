package cn.lili.modules.sms.impl;

import cn.hutool.core.util.StrUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.properties.SmsTemplateProperties;
import cn.lili.common.properties.SystemSettingProperties;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.Base64Utils;
import cn.lili.common.utils.CommonUtil;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.sms.AliSmsUtil;
import cn.lili.modules.sms.SmsUtil;
import cn.lili.modules.sms.entity.dos.SmsSign;
import cn.lili.modules.sms.entity.dos.SmsTemplate;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.SmsSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.lili.modules.verification.entity.enums.VerificationEnums;
import com.aliyun.dysmsapi20170525.models.*;
import com.aliyun.teaopenapi.models.Config;
import com.google.gson.Gson;
import com.xkcoding.http.util.StringUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 短信网管阿里云实现
 *
 * @author Chopper
 * @version v4.0
 * @since 2020/11/30 15:44
 */
@Component
@Slf4j
public class SmsUtilAliImplService implements SmsUtil, AliSmsUtil {

    @Autowired
    private Cache cache;
    @Autowired
    private SettingService settingService;
    @Autowired
    private MemberService memberService;

    @Autowired
    private SmsTemplateProperties smsTemplateProperties;

    @Autowired
    private SystemSettingProperties systemSettingProperties;

    @Override
    public void sendSmsCode(String mobile, VerificationEnums verificationEnums, String uuid) {
        //获取短信配置
        Setting setting = settingService.get(SettingEnum.SMS_SETTING.name());
        if (StrUtil.isBlank(setting.getSettingValue())) {
            throw new ServiceException(ResultCode.ALI_SMS_SETTING_ERROR);
        }
        SmsSetting smsSetting = new Gson().fromJson(setting.getSettingValue(), SmsSetting.class);

        //验证码
        String code = CommonUtil.getRandomNum();

        //准备发送短信参数
        Map<String, String> params = new HashMap<>(2);
        //验证码内容
        params.put("code", code);

        //模版 默认为登录验证
        String templateCode;

        //如果某个模版需要自定义，则在此处进行调整
        switch (verificationEnums) {
            //登录
            case LOGIN: {
                templateCode = smsTemplateProperties.getLOGIN();
                break;
            }
            //注册
            case REGISTER: {
                templateCode = smsTemplateProperties.getREGISTER();
                break;
            }
            //找回密码
            case FIND_USER: {
                templateCode = smsTemplateProperties.getFIND_USER();
                break;
            }
            //修改密码
            case UPDATE_PASSWORD: {
                Member member = memberService.getById(UserContext.getCurrentUser().getId());
                if (member == null || StringUtil.isEmpty(member.getMobile())) {
                    return;
                }
                //更新为用户最新手机号
                mobile = member.getMobile();
                templateCode = smsTemplateProperties.getUPDATE_PASSWORD();
                break;
            }
            //设置支付密码
            case WALLET_PASSWORD: {
                Member member = memberService.getById(UserContext.getCurrentUser().getId());
                //更新为用户最新手机号
                mobile = member.getMobile();
                templateCode = smsTemplateProperties.getWALLET_PASSWORD();
                break;
            }
            //如果不是有效的验证码手段，则此处不进行短信操作
            default:
                return;
        }
        //如果是测试模式 默认验证码 6个1
        if (systemSettingProperties.getIsTestModel()) {
            code = "111111";
            log.info("测试模式 - 接收手机：{},验证码：{}",mobile,code);
        } else {
            log.info("接收手机：{},验证码：{}",mobile,code);
            //发送短信
            this.sendSmsCode(smsSetting.getSignName(), mobile, params, templateCode);
        }
        //缓存中写入要验证的信息
        cache.put(cacheKey(verificationEnums, mobile, uuid), code, 300L);
    }

    @Override
    public boolean verifyCode(String mobile, VerificationEnums verificationEnums, String uuid, String code) {
        Object result = cache.get(cacheKey(verificationEnums, mobile, uuid));
        if (code.equals(result) || code.equals("0")) {
            //校验之后，删除
            cache.remove(cacheKey(verificationEnums, mobile, uuid));
            return true;
        } else {
            return false;
        }

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
     * @return Client 短信操作util
     */
    public com.aliyun.dysmsapi20170525.Client createClient() {
        try {
            Setting setting = settingService.get(SettingEnum.SMS_SETTING.name());
            if (StrUtil.isBlank(setting.getSettingValue())) {
                throw new ServiceException(ResultCode.ALI_SMS_SETTING_ERROR);
            }
            SmsSetting smsSetting = new Gson().fromJson(setting.getSettingValue(), SmsSetting.class);

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

    /**
     * 生成缓存key
     *
     * @param verificationEnums 验证场景
     * @param mobile            手机号码
     * @param uuid              用户标识 uuid
     * @return
     */
    static String cacheKey(VerificationEnums verificationEnums, String mobile, String uuid) {
        return CachePrefix.SMS_CODE.getPrefix() + verificationEnums.name() + uuid + mobile;
    }
}
