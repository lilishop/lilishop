package cn.lili.common.properties;

import jakarta.validation.constraints.AssertTrue;
import jakarta.validation.constraints.NotBlank;
import io.jsonwebtoken.io.Decoders;
import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;
import org.springframework.validation.annotation.Validated;

/**
 * token过期配置
 *
 * @author Chopper
 */
@Data
@Configuration
@Validated
@ConfigurationProperties(prefix = "lili.jwt-setting")
public class JWTTokenProperties {


    /**
     * Base64-encoded JWT signing key. It must be supplied by the deployment.
     */
    @NotBlank(message = "lili.jwt-setting.secret must be configured")
    private String secret;

    @AssertTrue(message = "lili.jwt-setting.secret must be Base64 encoded key with at least 32 bytes")
    public boolean isSecretKeySizeValid() {
        if (secret == null || secret.isBlank()) {
            return true;
        }
        try {
            return Decoders.BASE64.decode(secret).length >= 32;
        } catch (IllegalArgumentException exception) {
            return false;
        }
    }

    /**
     * token默认过期时间
     */
    private long tokenExpireTime = 60;
}
