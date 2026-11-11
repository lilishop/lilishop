package cn.lili.modules.im.service;

import cn.hutool.core.util.StrUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.ImSetting;
import cn.lili.modules.system.entity.dto.OssSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

/**
 * 客服图片独立 OSS 配置读取与校验，统一用于后台保存及实际上传，不回退到通用桶。
 * @author Bulbasaur
 * @since 2026-11-09
 */
@Service
@RequiredArgsConstructor
public class ImImageStorageSettings {
    private final SettingService settingService;

    /**
     * 规范化并校验配置；允许四项均为空以单独保存客服地址，部分填写则拒绝。
     * @param value 客服设置
     */
    public void validate(ImSetting value) {
        value.setImageOssBucket(StrUtil.trimToEmpty(value.getImageOssBucket()));
        value.setImageOssEndpoint(StrUtil.trimToEmpty(value.getImageOssEndpoint()));
        value.setImageOssAccessKeyId(StrUtil.trimToEmpty(value.getImageOssAccessKeyId()));
        value.setImageOssAccessKeySecret(StrUtil.trimToEmpty(value.getImageOssAccessKeySecret()));
        if (StrUtil.isAllBlank(value.getImageOssBucket(), value.getImageOssEndpoint(),
                value.getImageOssAccessKeyId(), value.getImageOssAccessKeySecret())) return;
        if (StrUtil.hasBlank(value.getImageOssBucket(), value.getImageOssEndpoint(),
                value.getImageOssAccessKeyId(), value.getImageOssAccessKeySecret())) {
            throw new ServiceException("请完整填写客服图片 OSS 的 Bucket、Endpoint 和访问密钥");
        }
        if (!value.getImageOssBucket().matches("[a-z0-9][a-z0-9-]{1,61}[a-z0-9]")
                || !value.getImageOssEndpoint().matches("oss-[a-z0-9-]+\\.aliyuncs\\.com")) {
            throw new ServiceException("客服图片 OSS Bucket 或 Endpoint 格式不正确，节点不应包含协议或路径");
        }
        Setting common = settingService.get(SettingEnum.OSS_SETTING.name());
        if (common != null && StrUtil.isNotBlank(common.getSettingValue())) {
            OssSetting storage = JSONUtil.toBean(common.getSettingValue(), OssSetting.class);
            if (value.getImageOssBucket().equals(StrUtil.trim(storage.getAliyunOSSBucketName()))) {
                throw new ServiceException("客服图片必须使用独立 Bucket，不能与通用文件桶相同");
            }
        }
    }

    /**
     * 获取完整、有效的独立 OSS 配置；未配置时拒绝上传，不使用默认文件存储。
     * @return 仅供服务端存储插件使用的配置
     */
    public OssSetting getStorage() {
        Setting setting = settingService.get(SettingEnum.IM_SETTING.name());
        ImSetting value = setting == null || StrUtil.isBlank(setting.getSettingValue())
                ? new ImSetting() : JSONUtil.toBean(setting.getSettingValue(), ImSetting.class);
        validate(value);
        if (StrUtil.isBlank(value.getImageOssBucket())) {
            throw new ServiceException("请先在客服设置中配置客服图片专用 OSS");
        }
        OssSetting result = new OssSetting();
        result.setType("ALI_OSS");
        result.setAliyunOSSBucketName(value.getImageOssBucket());
        result.setAliyunOSSEndPoint(value.getImageOssEndpoint());
        result.setAliyunOSSAccessKeyId(value.getImageOssAccessKeyId());
        result.setAliyunOSSAccessKeySecret(value.getImageOssAccessKeySecret());
        return result;
    }
}
