package cn.lili.modules.wechat.serviceimpl;

import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.lili.modules.wechat.util.WechatAccessTokenUtil;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import static org.mockito.Mockito.*;

class WechatMPServiceImplOrderSwitchTest {
    private WechatMPServiceImpl service;
    private SettingService settings;
    private OrderService orders;
    private WechatAccessTokenUtil tokens;

    @BeforeEach
    void setUp() {
        service = new WechatMPServiceImpl();
        settings = mock(SettingService.class);
        orders = mock(OrderService.class);
        tokens = mock(WechatAccessTokenUtil.class);
        ReflectionTestUtils.setField(service, "settingService", settings);
        ReflectionTestUtils.setField(service, "orderService", orders);
        ReflectionTestUtils.setField(service, "wechatAccessTokenUtil", tokens);
    }

    @Test
    void disabledSkipsShippingAndReceiveWithoutCallingWechat() {
        configure("{\"wechatOrderAutoUploadEnabled\":false}");
        service.uploadShippingInfo("order-1");
        service.notifyConfirmReceive("order-1");
        verifyNoInteractions(orders, tokens);
        verify(settings, never()).get(SettingEnum.WECHAT_PAYMENT.name());
    }

    @Test
    void missingAndLegacySettingsPreserveExistingBehavior() {
        Order order = new Order();
        order.setClientType("PC");
        when(orders.getBySn("order-1")).thenReturn(order);
        String[] values = {null, "", "{}", "null", "{\"wechatOrderAutoUploadEnabled\":null}",
                "{\"wechatOrderAutoUploadEnabled\":true}"};
        for (String value : values) {
            configure(value);
            service.uploadShippingInfo("order-1");
            service.notifyConfirmReceive("order-1");
        }
        verify(orders, times(values.length * 2)).getBySn("order-1");
        // 开关开启仍保留小程序来源校验，PC 订单不会请求微信。
        verifyNoInteractions(tokens);
    }

    @Test
    void savedSwitchIsReadAgainOnNextOperation() {
        configure("{\"wechatOrderAutoUploadEnabled\":false}");
        service.uploadShippingInfo("order-1");
        verifyNoInteractions(orders);

        Order order = new Order();
        order.setClientType("PC");
        when(orders.getBySn("order-1")).thenReturn(order);
        configure("{\"wechatOrderAutoUploadEnabled\":true}");
        service.uploadShippingInfo("order-1");
        verify(orders).getBySn("order-1");

        configure("{\"wechatOrderAutoUploadEnabled\":false}");
        service.notifyConfirmReceive("order-1");
        verifyNoMoreInteractions(orders);
        verifyNoInteractions(tokens);
    }

    private void configure(String value) {
        Setting setting = null;
        if (value != null) {
            setting = new Setting();
            setting.setSettingValue(value);
        }
        when(settings.get(SettingEnum.ORDER_SETTING.name())).thenReturn(setting);
    }
}
