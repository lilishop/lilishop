package cn.lili.common.properties;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

/**
 * API地址配置
 * @author Chopper
 */
@Data
@Configuration
@ConfigurationProperties(prefix = "lili.api")
public class ApiProperties {


    /**
     * 买家api
     */
    private String buyer;

    /**
     * 管理端域名
     */
    private String store;

    /**
     * 管理端域名
     */
    private String manager;

    /**
     * 管理端域名
     */
    private String common;
}
