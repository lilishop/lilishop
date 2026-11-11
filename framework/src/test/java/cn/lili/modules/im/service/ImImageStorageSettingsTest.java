package cn.lili.modules.im.service;

import cn.hutool.json.JSONUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.ImSetting;
import cn.lili.modules.system.entity.dto.OssSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.when;

/**
 * 客服图片独立 OSS 配置校验及获取逻辑测试
 *
 * @author Bulbasaur
 * @since 2026-11-11
 */
@ExtendWith(MockitoExtension.class)
class ImImageStorageSettingsTest {

    @Mock
    private SettingService settingService;

    @InjectMocks
    private ImImageStorageSettings storageSettings;

    @Test
    @DisplayName("校验-全部为空允许跳过独立桶配置")
    void validateAllowsAllBlank() {
        ImSetting setting = new ImSetting();
        assertDoesNotThrow(() -> storageSettings.validate(setting));
    }

    @Test
    @DisplayName("校验-部分填写抛出异常")
    void validateThrowsWhenPartialBlank() {
        ImSetting setting = new ImSetting();
        setting.setImageOssBucket("im-bucket");
        setting.setImageOssEndpoint("oss-cn-beijing.aliyuncs.com");

        ServiceException ex = assertThrows(ServiceException.class, () -> storageSettings.validate(setting));
        assertThat(ex.getMessage()).contains("请完整填写客服图片 OSS 的 Bucket、Endpoint 和访问密钥");
    }

    @Test
    @DisplayName("校验-Bucket或Endpoint格式非法抛出异常")
    void validateThrowsWhenFormatInvalid() {
        ImSetting invalidBucket = new ImSetting();
        invalidBucket.setImageOssBucket("-invalid_bucket-");
        invalidBucket.setImageOssEndpoint("oss-cn-beijing.aliyuncs.com");
        invalidBucket.setImageOssAccessKeyId("ak");
        invalidBucket.setImageOssAccessKeySecret("sk");

        ServiceException ex1 = assertThrows(ServiceException.class, () -> storageSettings.validate(invalidBucket));
        assertThat(ex1.getMessage()).contains("格式不正确");

        ImSetting invalidEndpoint = new ImSetting();
        invalidEndpoint.setImageOssBucket("valid-bucket");
        invalidEndpoint.setImageOssEndpoint("https://oss-cn-beijing.aliyuncs.com/");
        invalidEndpoint.setImageOssAccessKeyId("ak");
        invalidEndpoint.setImageOssAccessKeySecret("sk");

        ServiceException ex2 = assertThrows(ServiceException.class, () -> storageSettings.validate(invalidEndpoint));
        assertThat(ex2.getMessage()).contains("格式不正确");
    }

    @Test
    @DisplayName("校验-Bucket与通用文件桶相同时拒绝保存")
    void validateThrowsWhenBucketCollidesWithCommonOss() {
        ImSetting setting = new ImSetting();
        setting.setImageOssBucket("common-bucket");
        setting.setImageOssEndpoint("oss-cn-beijing.aliyuncs.com");
        setting.setImageOssAccessKeyId("ak");
        setting.setImageOssAccessKeySecret("sk");

        OssSetting common = new OssSetting();
        common.setAliyunOSSBucketName("common-bucket");
        Setting settingEntity = new Setting();
        settingEntity.setSettingValue(JSONUtil.toJsonStr(common));

        when(settingService.get(SettingEnum.OSS_SETTING.name())).thenReturn(settingEntity);

        ServiceException ex = assertThrows(ServiceException.class, () -> storageSettings.validate(setting));
        assertThat(ex.getMessage()).contains("不能与通用文件桶相同");
    }

    @Test
    @DisplayName("获取存储配置-未配置独立桶时抛出异常")
    void getStorageThrowsWhenNotConfigured() {
        when(settingService.get(SettingEnum.IM_SETTING.name())).thenReturn(null);

        ServiceException ex = assertThrows(ServiceException.class, () -> storageSettings.getStorage());
        assertThat(ex.getMessage()).contains("请先在客服设置中配置客服图片专用 OSS");
    }

    @Test
    @DisplayName("获取存储配置-配置完整正确返回AliOSS配置")
    void getStorageSuccess() {
        ImSetting imSetting = new ImSetting();
        imSetting.setImageOssBucket("im-bucket");
        imSetting.setImageOssEndpoint("oss-cn-beijing.aliyuncs.com");
        imSetting.setImageOssAccessKeyId("test-ak");
        imSetting.setImageOssAccessKeySecret("test-sk");

        Setting settingEntity = new Setting();
        settingEntity.setSettingValue(JSONUtil.toJsonStr(imSetting));
        when(settingService.get(SettingEnum.IM_SETTING.name())).thenReturn(settingEntity);

        OssSetting oss = storageSettings.getStorage();
        assertThat(oss.getType()).isEqualTo("ALI_OSS");
        assertThat(oss.getAliyunOSSBucketName()).isEqualTo("im-bucket");
        assertThat(oss.getAliyunOSSEndPoint()).isEqualTo("oss-cn-beijing.aliyuncs.com");
        assertThat(oss.getAliyunOSSAccessKeyId()).isEqualTo("test-ak");
        assertThat(oss.getAliyunOSSAccessKeySecret()).isEqualTo("test-sk");
    }
}
