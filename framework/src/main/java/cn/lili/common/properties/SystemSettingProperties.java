package cn.lili.common.properties;

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
     * <p>
     * PS:
     */
    private Integer sensitiveLevel = 0;


    public Boolean getDemoSite() {
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
        if (sensitiveLevel == null) {
            return 0;
        }
        return sensitiveLevel;
    }
}
