package cn.lili.modules.system.serviceimpl;

import cn.hutool.json.JSONUtil;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.WithdrawalSetting;
import cn.lili.modules.system.entity.dto.BaseSetting;
import cn.lili.modules.system.entity.dto.PaidMemberSetting;
import cn.lili.modules.member.service.PaidMemberService;
import cn.lili.modules.distribution.support.DistributionSettingSupport;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.cache.Cache;
import cn.lili.modules.system.service.SettingService;
import jakarta.validation.Validation;
import jakarta.validation.ValidatorFactory;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.test.util.ReflectionTestUtils;
import static org.assertj.core.api.Assertions.*;
import static org.mockito.Mockito.*;

/**
 * 验证提现及公众号配置统一转换后的类型、历史空值兼容和保存约束。
 * @author Bulbasaur
 * @since 2026-11-05
 */
class SettingConfigServiceTest {
    /** 实际校验器工厂，用于覆盖 DTO 声明的业务约束。 */
    private static final ValidatorFactory FACTORY = Validation.buildDefaultValidatorFactory();
    private SettingConfigServiceImpl service;
    private SettingService settings;

    /** 注入真实约束校验器，配置存储使用模拟服务。 */
    @BeforeEach void setup() {
        service = new SettingConfigServiceImpl();
        settings = mock(SettingService.class);
        ReflectionTestUtils.setField(service, "settingService", settings);
        ReflectionTestUtils.setField(service, "validator", FACTORY.getValidator());
        ReflectionTestUtils.setField(service, "cache", mock(Cache.class));
    }

    /** 释放测试创建的校验器资源。 */
    @AfterAll static void closeFactory() { FACTORY.close(); }

    /** 捕获最终存储 JSON，检查服务端实际保存的数据类型。 */
    private String savedValue() {
        var captor = ArgumentCaptor.forClass(Setting.class);
        verify(settings).saveUpdate(captor.capture());
        return captor.getValue().getSettingValue();
    }

    /** 字符串历史配置保存后恢复数字和布尔类型，空值不再写成字符串 null。 */
    @Test void withdrawalUsesTypedFields() {
        service.saveConfig(SettingEnum.WITHDRAWAL_SETTING, "{\"minPrice\":\"10.50\",\"fee\":\"null\",\"apply\":\"false\",\"type\":\"WECHAT\",\"wechatAppId\":\" wx123 \"}");
        var json = JSONUtil.parseObj(savedValue());
        assertThat(json.get("minPrice")).isInstanceOf(Number.class);
        assertThat(json.get("apply")).isEqualTo(false);
        assertThat(json.get("fee")).isNull();
        assertThat(json.getStr("wechatAppId")).isEqualTo("wx123");
    }

    /** 读取旧的空值配置不触发保存约束，也不会把空值转换为文本。 */
    @Test void oldBlankWithdrawalStillReads() {
        Setting old = new Setting(); old.setSettingValue("{\"minPrice\":\"null\",\"wechatAppId\":\"null\"}");
        when(settings.get(SettingEnum.WITHDRAWAL_SETTING.name())).thenReturn(old);
        WithdrawalSetting value = (WithdrawalSetting) service.getConfig(SettingEnum.WITHDRAWAL_SETTING);
        assertThat(value.getMinPrice()).isNull();
        assertThat(value.getWechatAppId()).isNull();
    }

    /** 负数和非法金额不能经通用设置接口写入。 */
    @Test void invalidWithdrawalAmountIsRejected() {
        for (String amount : new String[]{"-1", "\"abc\"", "null"}) {
            assertThatThrownBy(() -> service.saveConfig(SettingEnum.WITHDRAWAL_SETTING, "{\"minPrice\":" + amount + "}"))
                    .isInstanceOf(cn.lili.common.exception.ServiceException.class);
        }
        verify(settings, never()).saveUpdate(any());
    }

    /** 开启公众号入口时必须填写链接；域名及密钥长度继续由服务端约束。 */
    @Test void invalidOfficialAccountConfigIsRejected() {
        for (String json : new String[]{"{\"enabled\":true}", "{\"articleUrl\":\"https://example.com/\"}", "{\"encodingAesKey\":\"short\"}"}) {
            assertThatThrownBy(() -> service.saveConfig(SettingEnum.WECHAT_OFFICIAL_ACCOUNT_SETTING, json))
                    .isInstanceOf(cn.lili.common.exception.ServiceException.class);
        }
        verify(settings, never()).saveUpdate(any());
    }

    /** 正常配置去除首尾空格，校验用派生属性不得写入 JSON。 */
    @Test void officialAccountStoresOnlyBusinessFields() {
        service.saveConfig(SettingEnum.WECHAT_OFFICIAL_ACCOUNT_SETTING, "{\"enabled\":true,\"articleUrl\":\" https://mp.weixin.qq.com/s/test \",\"encodingAesKey\":\"null\"}");
        var json = JSONUtil.parseObj(savedValue());
        assertThat(json.getStr("articleUrl")).isEqualTo("https://mp.weixin.qq.com/s/test");
        assertThat(json.get("enabled")).isEqualTo(true);
        assertThat(json.get("encodingAesKey")).isNull();
        assertThat(json.containsKey("articleConfigured")).isFalse();
    }

    /** 未配置时直接返回原有默认 DTO，不带控制层响应包装。 */
    @Test void missingConfigReturnsDefaultDto() {
        assertThat(service.getConfig(SettingEnum.BASE_SETTING)).isInstanceOf(BaseSetting.class);
    }

    /** 付费会员的读写继续经过专用服务，不能直接绕过关联商家及商品校验。 */
    @Test void paidMemberDelegatesToBusinessService() {
        var paidMembers = mock(PaidMemberService.class);
        ReflectionTestUtils.setField(service, "paidMemberService", paidMembers);
        var config = new PaidMemberSetting();
        when(paidMembers.config()).thenReturn(config);
        assertThat(service.getConfig(SettingEnum.PAID_MEMBER_SETTING)).isSameAs(config);
        service.saveConfig(SettingEnum.PAID_MEMBER_SETTING, "{}");
        verify(paidMembers).saveConfig(any(PaidMemberSetting.class));
        verify(settings, never()).saveUpdate(any());
    }

    /** 分销过滤继续调用已有规则，保存仍复用底层配置存储及缓存失效。 */
    @Test void distributionKeepsExistingNormalization() {
        var distribution = mock(DistributionSettingSupport.class);
        ReflectionTestUtils.setField(service, "distributionSettingSupport", distribution);
        service.saveConfig(SettingEnum.DISTRIBUTION_SETTING, "{}");
        verify(distribution).normalize(any());
        verify(settings).saveUpdate(any());
    }
}
