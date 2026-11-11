package cn.lili.security;

import cn.hutool.core.util.StrUtil;
import cn.lili.cache.Cache;
import cn.lili.common.security.enums.SecurityEnum;
import cn.lili.common.properties.IgnoredUrlsProperties;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.authentication.www.BasicAuthenticationFilter;
import org.springframework.util.PatternMatchUtils;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * IM客服模块认证过滤器，负责提取请求头或参数中的访问令牌并进行服务端会话解析
 *
 * @author Bulbasaur
 * @since 2026-11-11
 */
@Slf4j
public class ImAuthenticationFilter extends BasicAuthenticationFilter {

    private final Cache cache;
    private final IgnoredUrlsProperties ignoredUrlsProperties;

    /**
     * 初始化IM认证过滤器
     *
     * @param authenticationManager 认证管理器
     * @param cache                 通用缓存
     * @param ignoredUrlsProperties 忽略鉴权URL配置
     */
    public ImAuthenticationFilter(AuthenticationManager authenticationManager,
                                  Cache cache,
                                  IgnoredUrlsProperties ignoredUrlsProperties) {
        super(authenticationManager);
        this.cache = cache;
        this.ignoredUrlsProperties = ignoredUrlsProperties;
    }

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain chain)
            throws IOException, ServletException {

        String requestUri = request.getRequestURI();
        if (ignoredUrlsProperties != null && ignoredUrlsProperties.getUrls() != null) {
            for (String url : ignoredUrlsProperties.getUrls()) {
                if (PatternMatchUtils.simpleMatch(url, requestUri)) {
                    chain.doFilter(request, response);
                    return;
                }
            }
        }

        // 优先从 Header 提取 accessToken，未获取到时兼容 Query 参数
        String jwt = request.getHeader(SecurityEnum.HEADER_TOKEN.getValue());
        if (StrUtil.isBlank(jwt)) {
            jwt = request.getParameter(SecurityEnum.HEADER_TOKEN.getValue());
        }
        if (StrUtil.isBlank(jwt)) {
            chain.doFilter(request, response);
            return;
        }

        try {
            UsernamePasswordAuthenticationToken authentication = getAuthentication(jwt);
            if (authentication != null) {
                SecurityContextHolder.getContext().setAuthentication(authentication);
            }
        } catch (Exception e) {
            log.error("IM认证过滤器处理令牌异常", e);
        }
        chain.doFilter(request, response);
    }

    /**
     * 根据访问令牌解析用户上下文并构建安全认证主体
     *
     * @param jwt 访问令牌
     * @return 安全认证主体；若令牌非法或已失效则返回 null
     */
    private UsernamePasswordAuthenticationToken getAuthentication(String jwt) {
        try {
            AuthUser authUser = UserContext.getAuthUser(cache, jwt);
            if (authUser != null) {
                List<GrantedAuthority> auths = new ArrayList<>();
                if (authUser.getRole() != null) {
                    auths.add(new SimpleGrantedAuthority("ROLE_" + authUser.getRole().name()));
                }
                return new UsernamePasswordAuthenticationToken(authUser, null, auths);
            }
        } catch (Exception e) {
            log.error("IM认证解析Token失败", e);
        }
        return null;
    }
}
