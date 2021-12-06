package cn.lili.modules.sms;

import cn.lili.modules.verification.entity.enums.VerificationEnums;

import java.util.List;
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


}
