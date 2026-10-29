package cn.lili.common.properties;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.sensitive.SensitiveFilterScene;
import cn.lili.common.utils.SpringContextUtil;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.SecuritySetting;
import cn.lili.modules.system.entity.dto.SensitiveWordsSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

/**
 * 系统设置
 *
 * @author Chopper
 */
@Data
@Configuration
@ConfigurationProperties(prefix = "lili.system")
public class SystemSettingProperties {

    /**
     * 是否是演示站点
     */
    private Boolean isDemoSite = false;

    /**
     * 测试模式
     * 验证码短信为6个1
     */
    private Boolean isTestModel = false;

    /**
     * 脱敏级别：
     * 0：不做脱敏处理
     * 1：管理端用户手机号等信息脱敏
     * 2：商家端信息脱敏（为2时，表示管理端，商家端同时脱敏）
     */
    private Integer sensitiveLevel = 0;

    /**
     * 敏感词设置缓存时间（秒），兜底5分钟
     */
    private static final long SENSITIVE_WORDS_SETTING_CACHE_TIME = 300L;

    private SecuritySetting getSecuritySetting() {
        try {
            SettingService settingService = SpringContextUtil.getBean(SettingService.class);
            if (settingService != null) {
                Setting setting = settingService.get(SettingEnum.SECURITY_SETTING.name());
                if (setting != null && CharSequenceUtil.isNotBlank(setting.getSettingValue())) {
                    return JSONUtil.toBean(setting.getSettingValue(), SecuritySetting.class);
                }
            }
        } catch (Exception ignored) {
        }
        return null;
    }

    public Boolean getDemoSite() {
        SecuritySetting securitySetting = getSecuritySetting();
        if (securitySetting != null && securitySetting.getIsDemoSite() != null) {
            return securitySetting.getIsDemoSite();
        }
        if (isDemoSite == null) {
            return false;
        }
        return isDemoSite;
    }

    public Boolean getTestModel() {
        if (isTestModel == null) {
            return false;
        }
        return isTestModel;
    }

    public Integer getSensitiveLevel() {
        SecuritySetting securitySetting = getSecuritySetting();
        if (securitySetting != null && securitySetting.getSensitiveLevel() != null) {
            return securitySetting.getSensitiveLevel();
        }
        if (sensitiveLevel == null) {
            return 0;
        }
        return sensitiveLevel;
    }

    private SensitiveWordsSetting getSensitiveWordsSetting() {
        String settingKey = SettingEnum.SENSITIVE_WORDS_SETTING.name();
        String cacheKey = CachePrefix.SETTING.getPrefix(settingKey);
        try {
            Cache cache = SpringContextUtil.getBean(Cache.class);
            if (cache != null) {
                String cacheValue = cache.getString(cacheKey);
                if (CharSequenceUtil.isNotBlank(cacheValue)) {
                    return JSONUtil.toBean(cacheValue, SensitiveWordsSetting.class);
                }
            }
            SettingService settingService = SpringContextUtil.getBean(SettingService.class);
            if (settingService != null) {
                Setting setting = settingService.get(settingKey);
                if (setting != null && CharSequenceUtil.isNotBlank(setting.getSettingValue())) {
                    if (cache != null) {
                        cache.put(cacheKey, setting.getSettingValue(), SENSITIVE_WORDS_SETTING_CACHE_TIME);
                    }
                    return JSONUtil.toBean(setting.getSettingValue(), SensitiveWordsSetting.class);
                }
            }
        } catch (Exception ignored) {
        }
        return null;
    }

    /**
     * 判断某个场景分组是否开启敏感词过滤
     * 设置不存在或解析失败时默认开启，保持现状
     *
     * @param scene 敏感词过滤场景分组
     * @return true-开启过滤，false-关闭过滤
     */
    public boolean isSensitiveFilterEnabled(SensitiveFilterScene scene) {
        SensitiveWordsSetting sensitiveWordsSetting = getSensitiveWordsSetting();
        if (sensitiveWordsSetting == null) {
            return true;
        }
        if (Boolean.FALSE.equals(sensitiveWordsSetting.getEnabled())) {
            return false;
        }
        if (scene == null) {
            return true;
        }
        return sensitiveWordsSetting.getDisabledGroups() == null
                || !sensitiveWordsSetting.getDisabledGroups().contains(scene.name());
    }
}
