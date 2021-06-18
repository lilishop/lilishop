package cn.lili.config.properties;

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
public class SystemSetting {


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
     * 授权信息
     */
    private String licences = "";
}
