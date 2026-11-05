package cn.lili.modules.sms;

import cn.lili.modules.sms.impl.SmsUtilAliImplService;
import cn.lili.modules.sms.plugin.SmsPlugin;
import cn.lili.modules.sms.plugin.SmsPluginFactory;
import cn.lili.cache.Cache;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.lili.modules.verification.entity.enums.VerificationEnums;
import org.mockito.ArgumentCaptor;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 通知类短信发送（带模板参数）
 */
class SmsUtilAliImplServiceTest {

    private SmsUtilAliImplService service;
    private SmsPluginFactory smsPluginFactory;
    private SmsPlugin smsPlugin;

    /**
     * 初始化当前用例的模拟依赖及测试数据。
     */
    @BeforeEach
    void setUp() {
        service = new SmsUtilAliImplService();
        smsPluginFactory = mock(SmsPluginFactory.class);
        smsPlugin = mock(SmsPlugin.class);
        ReflectionTestUtils.setField(service, "smsPluginFactory", smsPluginFactory);
        when(smsPluginFactory.smsPlugin()).thenReturn(smsPlugin);
    }

    /**
     * 验证通知短信发送委托短信插件处理。
     */
    @Test
    void sendNoticeSmsDelegatesToPlugin() {
        Map<String, String> params = Map.of("order_sn", "SN123");
        service.sendNoticeSms("商城平台", "13800000000", "SMS_001", params);
        verify(smsPlugin).sendSmsCode("商城平台", "13800000000", params, "SMS_001");
    }

    /** 验证登录验证码仅依赖短信配置、发送插件及缓存。 */
    @Test
    void loginCodeUsesSettingsAndCanBeVerifiedWithoutSmsManagementTables() {
        SettingService settings = mock(SettingService.class);
        Cache<Object> cache = mock(Cache.class);
        Setting setting = new Setting();
        setting.setSettingValue("{\"signName\":\"商城平台\",\"loginTemplateCode\":\"SMS_LOGIN\",\"isTestModel\":false}");
        when(settings.get(SettingEnum.SMS_SETTING.name())).thenReturn(setting);
        ReflectionTestUtils.setField(service, "settingService", settings);
        ReflectionTestUtils.setField(service, "cache", cache);

        service.sendSmsCode("13800000000", VerificationEnums.LOGIN, "request-id");

        ArgumentCaptor<Map<String, String>> parameters = ArgumentCaptor.forClass(Map.class);
        verify(smsPlugin).sendSmsCode(eq("商城平台"), eq("13800000000"), parameters.capture(), eq("SMS_LOGIN"));
        String code = parameters.getValue().get("code");
        assertThat(code).isNotBlank();
        ArgumentCaptor<Object> cacheKey = ArgumentCaptor.forClass(Object.class);
        verify(cache).put(cacheKey.capture(), eq(code), eq(300L));
        when(cache.get(cacheKey.getValue())).thenReturn(code);
        assertThat(service.verifyCode("13800000000", VerificationEnums.LOGIN, "request-id", code)).isTrue();
        verify(cache).remove(cacheKey.getValue());
    }
}
