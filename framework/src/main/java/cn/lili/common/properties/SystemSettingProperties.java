package cn.lili.common.properties;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.utils.SpringContextUtil;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.SecuritySetting;
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
}
