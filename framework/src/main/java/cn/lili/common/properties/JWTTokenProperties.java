package cn.lili.common.properties;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

/**
 * token过期配置
 *
 * @author Chopper
 */
@Data
@Configuration
@ConfigurationProperties(prefix = "lili.jwt-setting")
public class JWTTokenProperties {


    /**
     * token默认过期时间
     */
    private long tokenExpireTime = 60;
}
