package cn.lili.common.security.context;

import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.enums.SecurityEnum;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.security.token.SecretKeyUtil;
import cn.lili.common.utils.SpringContextUtil;
import com.google.gson.Gson;
import io.jsonwebtoken.Jwts;
import jakarta.servlet.http.HttpServletRequest;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.springframework.context.ApplicationContext;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import java.util.Collections;
import java.util.Date;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * 用户上下文单元测试
 *
 * @author Bulbasaur
 * @since 2026-11-11
 */
public class UserContextTest {

    @AfterEach
    public void tearDown() {
        SecurityContextHolder.clearContext();
        RequestContextHolder.resetRequestAttributes();
    }

    /**
     * 测试优先从 Spring Security 上下文获取已通过 Filter 校验的用户
     */
    @Test
    @DisplayName("测试从已鉴权的 SecurityContextHolder 获取用户")
    public void testGetCurrentUserFromSecurityContext() {
        AuthUser expectedUser = new AuthUser();
        expectedUser.setUsername("securityUser");
        expectedUser.setId("1001");
        expectedUser.setRole(UserEnums.MEMBER);

        UsernamePasswordAuthenticationToken auth =
                new UsernamePasswordAuthenticationToken("securityUser", null, Collections.emptyList());
        auth.setDetails(expectedUser);

        SecurityContext securityContext = mock(SecurityContext.class);
        when(securityContext.getAuthentication()).thenReturn(auth);
        SecurityContextHolder.setContext(securityContext);

        AuthUser currentUser = UserContext.getCurrentUser();
        Assertions.assertNotNull(currentUser);
        Assertions.assertEquals("securityUser", currentUser.getUsername());
        Assertions.assertEquals("1001", currentUser.getId());
    }

    /**
     * 测试免登录接口场景：已注销或 Redis 无会话的 Token 获取用户返回 null
     */
    @Test
    @DisplayName("测试免登录白名单接口且 Redis 中无会话时返回 null")
    public void testGetCurrentUserWhenTokenRevokedInRedis() {
        AuthUser tokenUser = new AuthUser();
        tokenUser.setUsername("revokedUser");
        tokenUser.setId("2002");
        tokenUser.setRole(UserEnums.MEMBER);

        String validSignedToken = Jwts.builder()
                .claim(SecurityEnum.USER_CONTEXT.getValue(), new Gson().toJson(tokenUser))
                .subject("revokedUser")
                .expiration(new Date(System.currentTimeMillis() + 60000))
                .signWith(SecretKeyUtil.generalKey())
                .compact();

        // 模拟 HTTP Request Header
        HttpServletRequest request = mock(HttpServletRequest.class);
        when(request.getHeader(SecurityEnum.HEADER_TOKEN.getValue())).thenReturn(validSignedToken);
        ServletRequestAttributes attributes = new ServletRequestAttributes(request);
        RequestContextHolder.setRequestAttributes(attributes);

        // 模拟 SpringContextUtil 返回 Cache，但 Redis 中已无该 Token（例如已退出登录）
        ApplicationContext applicationContext = mock(ApplicationContext.class);
        Cache cache = mock(Cache.class);
        when(cache.hasKey(anyString())).thenReturn(false);
        when(applicationContext.getBean(Cache.class)).thenReturn(cache);

        try (MockedStatic<SpringContextUtil> springContextUtilMock = Mockito.mockStatic(SpringContextUtil.class)) {
            springContextUtilMock.when(SpringContextUtil::getApplicationContext).thenReturn(applicationContext);
            springContextUtilMock.when(() -> SpringContextUtil.getBean(Cache.class)).thenReturn(cache);

            AuthUser currentUser = UserContext.getCurrentUser();
            // 因为 Redis 中没有会话记录，必须被判定为未登录，返回 null
            Assertions.assertNull(currentUser);
        }
    }

    /**
     * 测试免登录接口场景：Redis 中会话有效时正常识别用户
     */
    @Test
    @DisplayName("测试免登录白名单接口且 Redis 中会话有效时返回用户")
    public void testGetCurrentUserWhenTokenValidInRedis() {
        AuthUser tokenUser = new AuthUser();
        tokenUser.setUsername("validUser");
        tokenUser.setId("3003");
        tokenUser.setRole(UserEnums.MEMBER);

        String validSignedToken = Jwts.builder()
                .claim(SecurityEnum.USER_CONTEXT.getValue(), new Gson().toJson(tokenUser))
                .subject("validUser")
                .expiration(new Date(System.currentTimeMillis() + 60000))
                .signWith(SecretKeyUtil.generalKey())
                .compact();

        HttpServletRequest request = mock(HttpServletRequest.class);
        when(request.getHeader(SecurityEnum.HEADER_TOKEN.getValue())).thenReturn(validSignedToken);
        ServletRequestAttributes attributes = new ServletRequestAttributes(request);
        RequestContextHolder.setRequestAttributes(attributes);

        ApplicationContext applicationContext = mock(ApplicationContext.class);
        Cache cache = mock(Cache.class);
        // Redis 中存在该会话记录
        when(cache.hasKey(CachePrefix.ACCESS_TOKEN.getPrefix(UserEnums.MEMBER, "3003") + validSignedToken))
                .thenReturn(true);
        when(applicationContext.getBean(Cache.class)).thenReturn(cache);

        try (MockedStatic<SpringContextUtil> springContextUtilMock = Mockito.mockStatic(SpringContextUtil.class)) {
            springContextUtilMock.when(SpringContextUtil::getApplicationContext).thenReturn(applicationContext);
            springContextUtilMock.when(() -> SpringContextUtil.getBean(Cache.class)).thenReturn(cache);

            AuthUser currentUser = UserContext.getCurrentUser();
            Assertions.assertNotNull(currentUser);
            Assertions.assertEquals("validUser", currentUser.getUsername());
            Assertions.assertEquals("3003", currentUser.getId());
        }
    }
}
