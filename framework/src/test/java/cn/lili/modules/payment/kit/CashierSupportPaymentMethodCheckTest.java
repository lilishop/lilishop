package cn.lili.modules.payment.kit;

import cn.lili.common.enums.ClientTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.payment.entity.enums.CashierEnum;
import cn.lili.modules.payment.entity.enums.PaymentClientEnum;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.modules.payment.kit.dto.PayParam;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.payment.PaymentSupportSetting;
import cn.lili.modules.system.entity.dto.payment.dto.PaymentSupportItem;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.hutool.json.JSONUtil;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * 收银台支付方式后台开关安全校验测试
 *
 * @author Bulbasaur
 * @since 2026-11-11
 */
class CashierSupportPaymentMethodCheckTest {

    private CashierSupport cashierSupport;
    private SettingService settingService;

    @BeforeEach
    void setUp() {
        cashierSupport = new CashierSupport();
        settingService = mock(SettingService.class);
        ReflectionTestUtils.setField(cashierSupport, "settingService", settingService);
    }

    /**
     * 后台关闭余额支付时，直接调用支付接口必须被服务端拦截并抛出 PAY_NOT_SUPPORT
     */
    @Test
    @DisplayName("后台关闭余额支付时直接调用接口必须拦截")
    void paymentRejectsDisabledWalletMethod() {
        // 配置 H5 客户端仅开启 WECHAT 和 ALIPAY，不开启 WALLET
        PaymentSupportSetting supportSetting = new PaymentSupportSetting();
        PaymentSupportItem item = new PaymentSupportItem();
        item.setClient(ClientTypeEnum.H5.name());
        item.setSupports(List.of(PaymentMethodEnum.WECHAT.name(), PaymentMethodEnum.ALIPAY.name()));
        supportSetting.setPaymentSupportItems(List.of(item));

        Setting setting = new Setting();
        setting.setSettingValue(JSONUtil.toJsonStr(supportSetting));
        when(settingService.get(SettingEnum.PAYMENT_SUPPORT.name())).thenReturn(setting);

        PayParam payParam = new PayParam();
        payParam.setClientType(ClientTypeEnum.H5.name());
        payParam.setOrderType(CashierEnum.ORDER.name());
        payParam.setSn("O12345");

        HttpServletRequest request = mock(HttpServletRequest.class);
        HttpServletResponse response = mock(HttpServletResponse.class);

        // 尝试绕过前端界面直接调用余额支付
        assertThatThrownBy(() -> cashierSupport.payment(
                PaymentMethodEnum.WALLET,
                PaymentClientEnum.H5,
                request,
                response,
                payParam
        )).isInstanceOf(ServiceException.class)
          .hasFieldOrPropertyWithValue("resultCode", ResultCode.PAY_NOT_SUPPORT);
    }

    /**
     * 付费会员购买若使用非白名单支付方式，同样必须被服务端拦截
     */
    @Test
    @DisplayName("付费会员使用非允许支付方式直接调用接口必须拦截")
    void paymentRejectsUnsupportedMethodForPaidMember() {
        // 配置 PC 客户端开启 BANK_TRANSFER, WECHAT
        PaymentSupportSetting supportSetting = new PaymentSupportSetting();
        PaymentSupportItem item = new PaymentSupportItem();
        item.setClient(ClientTypeEnum.PC.name());
        item.setSupports(List.of(PaymentMethodEnum.BANK_TRANSFER.name(), PaymentMethodEnum.WECHAT.name()));
        supportSetting.setPaymentSupportItems(List.of(item));

        Setting setting = new Setting();
        setting.setSettingValue(JSONUtil.toJsonStr(supportSetting));
        when(settingService.get(SettingEnum.PAYMENT_SUPPORT.name())).thenReturn(setting);

        PayParam payParam = new PayParam();
        payParam.setClientType(ClientTypeEnum.PC.name());
        payParam.setOrderType(CashierEnum.PAID_MEMBER.name());
        payParam.setSn("PM12345");

        HttpServletRequest request = mock(HttpServletRequest.class);
        HttpServletResponse response = mock(HttpServletResponse.class);

        // 付费会员禁止使用 BANK_TRANSFER
        assertThatThrownBy(() -> cashierSupport.payment(
                PaymentMethodEnum.BANK_TRANSFER,
                PaymentClientEnum.NATIVE,
                request,
                response,
                payParam
        )).isInstanceOf(ServiceException.class)
          .hasFieldOrPropertyWithValue("resultCode", ResultCode.PAY_NOT_SUPPORT);
    }
}
