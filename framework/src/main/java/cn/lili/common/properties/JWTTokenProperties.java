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
     * 默认签名密钥（Base64 编码，长度为 256 位 / 32 字节，保持向后兼容）
     */
    public static final String DEFAULT_SECRET = "cuAihCz53DZRjZwbsGcZJ2Ai6At+T142uphtJMsk7iQ=";

    /**
     * JWT 签名密钥（生产环境建议通过配置文件或环境变量 LILI_JWT_SECRET 强制覆盖）
     */
    private String secret = DEFAULT_SECRET;

    /**
     * token默认过期时间
     */
    private long tokenExpireTime = 60;

    /**
     * 刷新令牌默认过期时间（单位：分钟，默认 30 天 = 43200 分钟）
     */
    private long refreshTokenExpireTime = 43200;
}
