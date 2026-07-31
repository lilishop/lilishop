/**
 * 作者：Mike
 * 日期：2026-07-31
 */
package cn.lili.common.properties;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

/**
 * 卡密 AES 加解密密钥，配置项 {@code lili.card-key.secret}。
 * 生产环境须更换默认值，与 PayKit 共用加解密实现。
 *
 * @author Mike
 * @date 2026-07-31
 */
@Data
@Configuration
@ConfigurationProperties(prefix = "lili.card-key")
public class CardKeyProperties {

    /**
     * AES 加密密钥
     */
    private String secret = "lilishop-card-key-default-secret";
}
