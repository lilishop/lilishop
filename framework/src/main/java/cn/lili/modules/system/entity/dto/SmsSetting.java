package cn.lili.modules.system.entity.dto;

import lombok.Data;

import java.io.Serializable;

/**
 * 短信配置
 * 这里在前台不做调整，方便客户直接把服务商的内容配置在我们平台
 *
 * @author Chopper
 * @since 2020/11/30 15:23
 */
@Data
public class SmsSetting implements Serializable {

    /**
     * 类型
     */
    private String type;

    /**
     * key
     */
    private String accessKeyId;
    /**
     * 密钥
     */
    private String accessSecret;
    /**
     * 签名
     */
    private String signName;


    /**
     * 腾讯云 用户的 SecretId
     */
    String tencentSecretId;
    /**
     * 腾讯云 用户的 SecretKey
     */
    String tencentSecretKey;
    /* 短信应用ID: 短信SdkAppId在 [短信控制台] 添加应用后生成的实际SdkAppId，示例如1400006666 */
    String tencentSdkAppId;
    /* 短信签名内容: 使用 UTF-8 编码，必须填写已审核通过的签名 */
    String tencentSignName;

    /**
     * 华为 APP_Key
     */
    String huaweiAppKey;
    /**
     * 华为 APP_Secret
     */
    String huaweiAppSecret;

    /**
     * 国内短信签名通道号或国际/港澳台短信通道号
     */
    String huaweiSender;
    /**
     * 签名名称
     */
    String huaweiSignature;
}
