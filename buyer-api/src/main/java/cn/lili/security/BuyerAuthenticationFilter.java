package cn.lili.security;

import cn.hutool.core.util.StrUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.properties.IgnoredUrlsProperties;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.enums.SecurityEnum;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.security.token.SecretKeyUtil;
import cn.lili.common.utils.ResponseUtil;
import com.google.gson.Gson;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.ExpiredJwtException;
import io.jsonwebtoken.Jwts;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.authentication.www.BasicAuthenticationFilter;
import org.springframework.util.PatternMatchUtils;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;


/**
 * 认证结果过滤器
 *
 * @author Chopper
 * @version v4.1
 * @since 2020/11/17 3:37 下午
 * @since
 */
@Slf4j
public class BuyerAuthenticationFilter extends BasicAuthenticationFilter {

    private final Cache cache;

    private final IgnoredUrlsProperties ignoredUrlsProperties;

    /**
     * 自定义构造器
     *
     * @param authenticationManager  认证管理器
     * @param cache                  缓存
     * @param ignoredUrlsProperties  忽略鉴权配置
     */
    public BuyerAuthenticationFilter(AuthenticationManager authenticationManager,
                                     Cache cache,
                                     IgnoredUrlsProperties ignoredUrlsProperties) {
        super(authenticationManager);
        this.cache = cache;
        this.ignoredUrlsProperties = ignoredUrlsProperties;
    }

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain chain)
            throws IOException, ServletException {

        // 忽略无需鉴权的路径，直接放行（与管理端保持一致）
        String requestUri = request.getRequestURI();
        for (String url : ignoredUrlsProperties.getUrls()) {
            if (PatternMatchUtils.simpleMatch(url, requestUri)) {
                chain.doFilter(request, response);
                return;
            }
        }

        //从header中获取jwt
        String jwt = request.getHeader(SecurityEnum.HEADER_TOKEN.getValue());
        //如果没有token 则return
        if (StrUtil.isBlank(jwt)) {
            chain.doFilter(request, response);
            return;
        }

        try {
            UsernamePasswordAuthenticationToken authentication = getAuthentication(jwt, response);
            if (authentication != null) {
                SecurityContextHolder.getContext().setAuthentication(authentication);
                chain.doFilter(request, response);
            } else if (!response.isCommitted()) {
                ResponseUtil.output(response, 403, ResponseUtil.resultMap(false, 403, "登录已失效，请重新登录"));
            }
        } catch (Exception e) {
            log.error("BuyerAuthenticationFilter-> member authentication exception:", e);
            if (!response.isCommitted()) {
                chain.doFilter(request, response);
            }
        }
    }

    /**
     * 解析用户
     *
     * @param jwt      token
     * @param response 响应
     * @return 认证信息
     */
    private UsernamePasswordAuthenticationToken getAuthentication(String jwt, HttpServletResponse response) {

        try {
            Claims claims =
                    Jwts.parser()
                            .verifyWith(SecretKeyUtil.generalKeyByDecoders())
                            .build()
                            .parseSignedClaims(jwt)
                            .getPayload();
            // 获取存储在claims中的用户信息
            String json = claims.get(SecurityEnum.USER_CONTEXT.getValue()).toString();
            AuthUser authUser = new Gson().fromJson(json, AuthUser.class);

            //校验redis中是否有权限
            if (cache.hasKey(CachePrefix.ACCESS_TOKEN.getPrefix(UserEnums.MEMBER, authUser.getId()) + jwt)) {
                //构造返回信息
                List<GrantedAuthority> auths = new ArrayList<>();
                auths.add(new SimpleGrantedAuthority("ROLE_" + authUser.getRole().name()));
                UsernamePasswordAuthenticationToken authentication = new UsernamePasswordAuthenticationToken(authUser.getUsername(), null, auths);
                authentication.setDetails(authUser);
                return authentication;
            }
        } catch (ExpiredJwtException e) {
            log.debug("user analysis exception:", e);
        } catch (Exception e) {
            log.error("user analysis exception:", e);
        }
        return null;
    }

}
