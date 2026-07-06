package cn.lili.modules.system.entity.dto;

import lombok.Data;

import java.io.Serializable;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
@Data
public class LiveSetting implements Serializable {
    /**
     * 直播推流域名
     */
    private String domain;
    /**
     * 小程序直播推流域名
     */
    private String mpPushDomain;

    /**
     * 直播拉流域名
     */
    private String pullDomain;

    /**
     * 直播应用名称
     */
    private String appName;
    /**
     * 直播流名称
     */
    private String streamName;
    /**
     * 密钥Id
     */
    private String secretId;
    /**
     * 密钥Key
     */
    private String secretKey;

    /**
     * 推流鉴权秘钥
     */
    private String key;
    /**
     * 直播回调地址
     */
    private String callBackUrl;

     /**
     * 直播IM SDK APPID
     */
    private String imSdkAppid;

     /**
     * 直播IM SDK 密钥
     */
    private String imSdkSecretKey;

}
