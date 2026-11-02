package cn.lili.modules.sms;

import cn.lili.modules.sms.impl.SmsUtilAliImplService;
import cn.lili.modules.sms.plugin.SmsPlugin;
import cn.lili.modules.sms.plugin.SmsPluginFactory;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Map;

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

    @BeforeEach
    void setUp() {
        service = new SmsUtilAliImplService();
        smsPluginFactory = mock(SmsPluginFactory.class);
        smsPlugin = mock(SmsPlugin.class);
        ReflectionTestUtils.setField(service, "smsPluginFactory", smsPluginFactory);
        when(smsPluginFactory.smsPlugin()).thenReturn(smsPlugin);
    }

    @Test
    void sendNoticeSmsDelegatesToPlugin() {
        Map<String, String> params = Map.of("order_sn", "SN123");
        service.sendNoticeSms("商城平台", "13800000000", "SMS_001", params);
        verify(smsPlugin).sendSmsCode("商城平台", "13800000000", params, "SMS_001");
    }
}
