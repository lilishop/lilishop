package cn.lili.common.security.token;

import cn.lili.common.properties.JWTTokenProperties;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.JwtException;
import io.jsonwebtoken.Jwts;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import javax.crypto.SecretKey;
import java.util.Date;

/**
 * JWT 密钥工具类单元测试
 *
 * @author Bulbasaur
 * @since 2026-11-11
 */
public class SecretKeyUtilTest {

    @AfterEach
    public void tearDown() {
        // 测试结束后重置回默认配置
        SecretKeyUtil secretKeyUtil = new SecretKeyUtil();
        secretKeyUtil.setJwtTokenProperties(new JWTTokenProperties());
    }

    /**
     * 测试默认密钥生成与向后兼容性
     */
    @Test
    @DisplayName("测试默认配置下的密钥生成")
    public void testDefaultKeyGeneration() {
        SecretKey key1 = SecretKeyUtil.generalKey();
        SecretKey key2 = SecretKeyUtil.generalKeyByDecoders();

        Assertions.assertNotNull(key1);
        Assertions.assertNotNull(key2);
        Assertions.assertEquals(key1.getAlgorithm(), key2.getAlgorithm());
        Assertions.assertArrayEquals(key1.getEncoded(), key2.getEncoded());
    }

    /**
     * 测试通过配置更换密钥并完成签名与验签
     */
    @Test
    @DisplayName("测试自定义密钥签名与验签")
    public void testCustomSecretSignAndVerify() {
        // 32 字节（256 位）Base64 密钥
        String customSecret = "MTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTI=";
        JWTTokenProperties properties = new JWTTokenProperties();
        properties.setSecret(customSecret);

        SecretKeyUtil secretKeyUtil = new SecretKeyUtil();
        secretKeyUtil.setJwtTokenProperties(properties);

        SecretKey key = SecretKeyUtil.generalKey();
        Assertions.assertNotNull(key);

        // 使用自定义密钥生成 Token
        String token = Jwts.builder()
                .subject("testUser")
                .expiration(new Date(System.currentTimeMillis() + 60000))
                .signWith(key)
                .compact();

        // 使用相同的自定义密钥验签
        Claims claims = Jwts.parser()
                .verifyWith(SecretKeyUtil.generalKeyByDecoders())
                .build()
                .parseSignedClaims(token)
                .getPayload();

        Assertions.assertEquals("testUser", claims.getSubject());

        // 使用默认旧密钥验签应该失败（签名不匹配或异常）
        SecretKey defaultKey = SecretKeyUtil.generalKey(JWTTokenProperties.DEFAULT_SECRET);
        Assertions.assertThrows(JwtException.class, () -> {
            Jwts.parser()
                    .verifyWith(defaultKey)
                    .build()
                    .parseSignedClaims(token)
                    .getPayload();
        });
    }

    /**
     * 测试非 Base64 或较短的密钥自动派生处理
     */
    @Test
    @DisplayName("测试普通明文及短密钥容错派生")
    public void testShortAndPlaintextSecret() {
        // 短明文字符串
        SecretKey key1 = SecretKeyUtil.generalKey("short-key");
        Assertions.assertNotNull(key1);

        String token = Jwts.builder()
                .subject("shortKeyUser")
                .signWith(key1)
                .compact();

        Claims claims = Jwts.parser()
                .verifyWith(key1)
                .build()
                .parseSignedClaims(token)
                .getPayload();

        Assertions.assertEquals("shortKeyUser", claims.getSubject());
    }
}
