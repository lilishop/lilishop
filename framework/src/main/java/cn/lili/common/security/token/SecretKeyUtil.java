package cn.lili.common.security.token;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.properties.JWTTokenProperties;
import cn.lili.common.utils.SpringContextUtil;
import io.jsonwebtoken.io.Decoders;
import io.jsonwebtoken.security.Keys;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.crypto.SecretKey;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Objects;

/**
 * SignWithUtil
 *
 * @author Chopper
 * @version v1.0
 * 2020-11-18 17:30
 */
@Component
public class SecretKeyUtil {

    private static volatile JWTTokenProperties jwtTokenProperties;
    private static volatile String lastSecret;
    private static volatile SecretKey cachedKey;

    @Autowired
    public void setJwtTokenProperties(JWTTokenProperties properties) {
        SecretKeyUtil.jwtTokenProperties = properties;
    }

    /**
     * 获取当前系统配置的签名密钥
     *
     * @return 密钥字符串
     */
    private static String getSecret() {
        if (jwtTokenProperties != null && CharSequenceUtil.isNotBlank(jwtTokenProperties.getSecret())) {
            return jwtTokenProperties.getSecret();
        }
        try {
            if (SpringContextUtil.getApplicationContext() != null) {
                JWTTokenProperties properties = SpringContextUtil.getBean(JWTTokenProperties.class);
                if (properties != null && CharSequenceUtil.isNotBlank(properties.getSecret())) {
                    return properties.getSecret();
                }
            }
        } catch (Exception ignored) {
        }
        return JWTTokenProperties.DEFAULT_SECRET;
    }

    /**
     * 构建或获取默认配置的 SecretKey
     *
     * @return 签名密钥
     */
    public static SecretKey generalKey() {
        return generalKeyByDecoders();
    }

    /**
     * 根据当前配置构建 SecretKey
     *
     * @return 签名密钥
     */
    public static SecretKey generalKeyByDecoders() {
        return generalKey(getSecret());
    }

    /**
     * 根据指定的密钥字符串生成 SecretKey，支持缓存提升高频验签性能
     *
     * @param secret 密钥明文或 Base64 字符串
     * @return 签名密钥
     */
    public static SecretKey generalKey(String secret) {
        if (CharSequenceUtil.isBlank(secret)) {
            secret = JWTTokenProperties.DEFAULT_SECRET;
        }
        SecretKey currentCachedKey = cachedKey;
        if (currentCachedKey != null && Objects.equals(secret, lastSecret)) {
            return currentCachedKey;
        }
        SecretKey newKey = buildKey(secret);
        lastSecret = secret;
        cachedKey = newKey;
        return newKey;
    }

    /**
     * 内部构建 SecretKey 实例（优先 Base64 解码，不足 256 位时采用 SHA-256 派生）
     *
     * @param secret 密钥字符串
     * @return 签名密钥
     */
    private static SecretKey buildKey(String secret) {
        byte[] keyBytes;
        try {
            keyBytes = Decoders.BASE64.decode(secret);
            if (keyBytes.length < 32) {
                keyBytes = secret.getBytes(StandardCharsets.UTF_8);
            }
        } catch (Exception e) {
            keyBytes = secret.getBytes(StandardCharsets.UTF_8);
        }
        if (keyBytes.length < 32) {
            try {
                MessageDigest md = MessageDigest.getInstance("SHA-256");
                keyBytes = md.digest(secret.getBytes(StandardCharsets.UTF_8));
            } catch (NoSuchAlgorithmException e) {
                throw new IllegalStateException("SHA-256 algorithm not available", e);
            }
        }
        return Keys.hmacShaKeyFor(keyBytes);
    }
}
