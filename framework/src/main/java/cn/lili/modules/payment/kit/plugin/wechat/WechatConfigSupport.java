package cn.lili.modules.payment.kit.plugin.wechat;

import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.payment.WechatPaymentSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import com.wechat.pay.java.core.Config;
import com.wechat.pay.java.core.RSAAutoCertificateConfig;
import com.wechat.pay.java.core.RSAPublicKeyConfig;
import com.wechat.pay.java.core.notification.NotificationConfig;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 微信支付配置支持组件
 * 负责微信支付商户基础配置读取、SDK RSA 自动证书与公钥配置实例构建，以及异步通知验签配置对象的统一构造与复用。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Slf4j
@Component
public class WechatConfigSupport {

    @Autowired
    private SettingService settingService;

    /**
     * 获取微信支付商户系统配置
     *
     * @return 微信支付配置 DTO
     * @throws ServiceException 当系统尚未配置微信支付时抛出
     */
    public WechatPaymentSetting getPaymentSetting() {
        try {
            Setting systemSetting = settingService.get(SettingEnum.WECHAT_PAYMENT.name());
            return JSONUtil.toBean(systemSetting.getSettingValue(), WechatPaymentSetting.class);
        } catch (Exception e) {
            log.error("微信支付配置读取失败或暂不支持", e);
            throw new ServiceException(ResultCode.PAY_NOT_SUPPORT);
        }
    }

    /**
     * 构建微信官方 SDK Config 实例
     * 自动从系统配置读取并按 publicType 分支适配证书或公钥模式
     *
     * @return 微信支付 Config 对象
     */
    public Config getWechatConfig() {
        return getWechatConfig(getPaymentSetting());
    }

    /**
     * 根据指定的配置对象构建微信官方 SDK Config 实例
     *
     * @param setting 微信支付设置
     * @return 微信支付 Config 对象
     */
    public Config getWechatConfig(WechatPaymentSetting setting) {
        if ("CERT".equals(setting.getPublicType())) {
            return getCertificateConfig(setting);
        } else {
            return getPublicKeyConfig(setting);
        }
    }

    /**
     * 构建异步回调验签所需的 NotificationConfig 实例
     *
     * @return 回调验签配置
     */
    public NotificationConfig getNotificationConfig() {
        return getNotificationConfig(getPaymentSetting());
    }

    /**
     * 根据指定的配置对象构建异步回调验签所需的 NotificationConfig 实例
     * 针对 CERT 模式使用 RSAAutoCertificateConfig，针对公钥模式使用 RSAPublicKeyConfig
     *
     * @param setting 微信支付设置
     * @return 回调验签配置
     */
    public NotificationConfig getNotificationConfig(WechatPaymentSetting setting) {
        if ("CERT".equals(setting.getPublicType())) {
            return new RSAAutoCertificateConfig.Builder()
                    .merchantId(setting.getMchId())
                    .privateKey(setting.getApiclientKey())
                    .merchantSerialNumber(setting.getSerialNumber())
                    .apiV3Key(setting.getApiKey3())
                    .build();
        } else {
            return new RSAPublicKeyConfig.Builder()
                    .merchantId(setting.getMchId())
                    .apiV3Key(setting.getApiKey3())
                    .privateKey(setting.getApiclientKey())
                    .merchantSerialNumber(setting.getSerialNumber())
                    .publicKeyId(setting.getPublicId())
                    .publicKey(setting.getPublicKey())
                    .build();
        }
    }

    /**
     * 构建微信公钥配置
     *
     * @param setting 微信支付设置
     * @return 公钥配置
     */
    public RSAPublicKeyConfig getPublicKeyConfig(WechatPaymentSetting setting) {
        return new RSAPublicKeyConfig.Builder()
                .merchantId(setting.getMchId())
                .privateKey(setting.getApiclientKey())
                .publicKey(setting.getPublicKey())
                .publicKeyId(setting.getPublicId())
                .merchantSerialNumber(setting.getSerialNumber())
                .apiV3Key(setting.getApiKey3())
                .build();
    }

    /**
     * 构建微信自动证书下载配置
     *
     * @param setting 微信支付设置
     * @return 证书配置
     */
    public RSAAutoCertificateConfig getCertificateConfig(WechatPaymentSetting setting) {
        return new RSAAutoCertificateConfig.Builder()
                .merchantId(setting.getMchId())
                .privateKey(setting.getApiclientKey())
                .merchantSerialNumber(setting.getSerialNumber())
                .apiV3Key(setting.getApiKey3())
                .build();
    }
}
