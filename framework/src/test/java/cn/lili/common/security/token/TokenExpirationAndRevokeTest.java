package cn.lili.common.security.token;

import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.properties.JWTTokenProperties;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.enums.UserEnums;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.concurrent.TimeUnit;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 令牌过期时间与刷新令牌生命周期测试
 * 验证买家端短时 AccessToken (1天) 与长时 RefreshToken (30天) 生成与续期策略
 *
 * @author Bulbasaur
 * @since 2026-11-11
 */
@ExtendWith(MockitoExtension.class)
public class TokenExpirationAndRevokeTest {

    @Mock
    private JWTTokenProperties tokenProperties;

    @Mock
    private Cache cache;

    @InjectMocks
    private TokenUtil tokenUtil;

    @Test
    @DisplayName("测试买家端创建令牌时，AccessToken为1天(1440分)，RefreshToken为30天(43200分)")
    public void testBuyerTokenGenerationExpiration() {
        when(tokenProperties.getTokenExpireTime()).thenReturn(1440L);
        when(tokenProperties.getRefreshTokenExpireTime()).thenReturn(43200L);

        AuthUser authUser = new AuthUser("test_user", "1001", "昵称", "face.png", UserEnums.MEMBER);
        authUser.setLongTerm(false);

        Token token = tokenUtil.createToken(authUser);

        assertThat(token).isNotNull();
        assertThat(token.getAccessToken()).isNotEmpty();
        assertThat(token.getRefreshToken()).isNotEmpty();

        // 校验 AccessToken 存入 Redis 的过期时间为 1440 分钟（1天）
        verify(cache).put(
                eq(CachePrefix.ACCESS_TOKEN.getPrefix(UserEnums.MEMBER, "1001") + token.getAccessToken()),
                eq(1),
                eq(1440L),
                eq(TimeUnit.MINUTES)
        );

        // 校验 RefreshToken 存入 Redis 的过期时间为 43200 分钟（30天）
        verify(cache).put(
                eq(CachePrefix.REFRESH_TOKEN.getPrefix(UserEnums.MEMBER, "1001") + token.getRefreshToken()),
                eq(1),
                eq(43200L),
                eq(TimeUnit.MINUTES)
        );
    }

    @Test
    @DisplayName("测试买家端通过旧RefreshToken刷新令牌时，新RefreshToken继续续期30天并移除旧令牌")
    public void testBuyerRefreshTokenRotation() {
        when(tokenProperties.getTokenExpireTime()).thenReturn(1440L);
        when(tokenProperties.getRefreshTokenExpireTime()).thenReturn(43200L);

        AuthUser authUser = new AuthUser("test_user", "1001", "昵称", "face.png", UserEnums.MEMBER);
        authUser.setLongTerm(false);
        Token initialToken = tokenUtil.createToken(authUser);
        String oldRefreshToken = initialToken.getRefreshToken();

        // 模拟 Redis 中存在该旧的 RefreshToken
        when(cache.hasKey(CachePrefix.REFRESH_TOKEN.getPrefix(UserEnums.MEMBER, "1001") + oldRefreshToken))
                .thenReturn(true);

        Token refreshedToken = tokenUtil.refreshToken(oldRefreshToken);

        assertThat(refreshedToken).isNotNull();
        assertThat(refreshedToken.getAccessToken()).isNotEmpty();
        assertThat(refreshedToken.getRefreshToken()).isNotEmpty();

        // 校验新生成的 RefreshToken 继续被赋予 30 天（43200 分钟）寿命
        verify(cache).put(
                eq(CachePrefix.REFRESH_TOKEN.getPrefix(UserEnums.MEMBER, "1001") + refreshedToken.getRefreshToken()),
                eq(1),
                eq(43200L),
                eq(TimeUnit.MINUTES)
        );

        // 校验旧的 RefreshToken 被从 Redis 中清理作废
        verify(cache).remove(CachePrefix.REFRESH_TOKEN.getPrefix(UserEnums.MEMBER, "1001") + oldRefreshToken);
    }
}
