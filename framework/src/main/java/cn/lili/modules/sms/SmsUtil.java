package cn.lili.modules.sms;

import cn.lili.modules.verification.entity.enums.VerificationEnums;

import java.util.Map;

/**
 * 短信接口
 *
 * @author Chopper
 * @version v4.0
 * @since 2020/11/30 15:44
 */
public interface SmsUtil {


    /**
     * 验证码发送
     *
     * @param mobile            手机号
     * @param verificationEnums 验证码场景
     * @param uuid              用户标识uuid
     */
    void sendSmsCode(String mobile, VerificationEnums verificationEnums, String uuid);

    /**
     * 验证码验证
     *
     * @param mobile            手机号
     * @param verificationEnums 验证码场景
     * @param uuid              用户标识uuid
     * @param code              待验证code
     * @return 操作状态
     */
    boolean verifyCode(String mobile, VerificationEnums verificationEnums, String uuid, String code);


    /**
     * 通知类短信发送（单条，带模板参数）
     *
     * @param signName     签名
     * @param mobile       接收手机号
     * @param templateCode 模版code
     * @param param        模板参数（变量名需与短信服务商模板变量一致）
     */
    void sendNoticeSms(String signName, String mobile, String templateCode, Map<String, String> param);


}
