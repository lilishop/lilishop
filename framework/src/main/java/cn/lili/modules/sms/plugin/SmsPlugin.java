package cn.lili.modules.sms.plugin;

import cn.lili.modules.sms.entity.dos.SmsSign;
import cn.lili.modules.sms.entity.dos.SmsTemplate;
import cn.lili.modules.sms.entity.enums.SmsEnum;

import java.util.List;
import java.util.Map;

/**
 * 短信插件接口
 *
 * @author Bulbasaur
 * @since 2023-02-16
 */
public interface SmsPlugin {

    /**
     * 插件名称
     */
    SmsEnum pluginName();

    /**
     * 短信发送
     *
     * @param mobile       接收手机号
     * @param param        参数
     * @param templateCode 模版code
     * @param signName     签名名称
     */
    void sendSmsCode(String signName, String mobile, Map<String, String> param, String templateCode);

    /**
     * 短信批量发送
     *
     * @param mobile       接收手机号
     * @param signName     签名
     * @param templateCode 模版code
     */
    void sendBatchSms(String signName, List<String> mobile, String templateCode);


    /**
     * 申请短信签名
     *
     * @param smsSign 短信签名
     * @throws Exception 阿里短信签名错误
     */
    void addSmsSign(SmsSign smsSign) throws Exception;


    /**
     * 删除短信签名
     *
     * @param signName 签名名称
     * @throws Exception 阿里短信签名错误
     */
    void deleteSmsSign(String signName) throws Exception;

    /**
     * 查询短信签名申请状态
     *
     * @param signName 签名名称
     * @return 短信签名申请状态
     * @throws Exception 阿里短信签名错误
     */
    Map<String, Object> querySmsSign(String signName) throws Exception;

    /**
     * 修改未审核通过的短信签名，并重新提交审核。
     *
     * @param smsSign 短信签名
     * @throws Exception 阿里短信签名错误
     */
    void modifySmsSign(SmsSign smsSign) throws Exception;

    /**
     * 修改未审核通过的短信模板，并重新提交审核。
     *
     * @param smsTemplate 短信模板
     * @throws Exception 阿里短信签名错误
     */
    void modifySmsTemplate(SmsTemplate smsTemplate) throws Exception;

    /**
     * 查看短信模板
     *
     * @param templateCode 短信模板CODE
     * @return 短信模板
     * @throws Exception 阿里短信签名错误
     */
    Map<String, Object> querySmsTemplate(String templateCode) throws Exception;

    /**
     * 申请短信模板
     *
     * @param smsTemplate 短信模板
     * @return 短信模板
     * @throws Exception 阿里短信签名错误
     */
    String addSmsTemplate(SmsTemplate smsTemplate) throws Exception;

    /**
     * 删除短信模板
     *
     * @param templateCode 短信模板CODE
     * @throws Exception 阿里短信签名错误
     */
    void deleteSmsTemplate(String templateCode) throws Exception;
}
