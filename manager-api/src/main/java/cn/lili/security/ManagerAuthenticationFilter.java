package cn.lili.security;

import cn.hutool.core.util.StrUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.enums.PermissionEnum;
import cn.lili.common.security.enums.SecurityEnum;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.security.token.SecretKeyUtil;
import cn.lili.common.utils.ResponseUtil;
import cn.lili.modules.permission.service.MenuService;
import cn.lili.modules.system.token.ManagerTokenGenerate;
import com.google.gson.Gson;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.ExpiredJwtException;
import io.jsonwebtoken.JwtException;
import io.jsonwebtoken.Jwts;
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
import org.springframework.web.bind.annotation.RequestMethod;

import javax.naming.NoPermissionException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import cn.lili.common.properties.IgnoredUrlsProperties;

/**
 * 管理端token过滤
 *
 * @author Chopper
 */
@Slf4j
public class ManagerAuthenticationFilter extends BasicAuthenticationFilter {

    private final Cache cache;

    public final MenuService menuService;

    private final ManagerTokenGenerate managerTokenGenerate;

    private final IgnoredUrlsProperties ignoredUrlsProperties;

    public ManagerAuthenticationFilter(AuthenticationManager authenticationManager,
                                       MenuService menuService,
                                       ManagerTokenGenerate managerTokenGenerate,
                                       Cache cache,
                                       IgnoredUrlsProperties ignoredUrlsProperties) {
        super(authenticationManager);
        this.cache = cache;
        this.menuService = menuService;
        this.managerTokenGenerate = managerTokenGenerate;
        this.ignoredUrlsProperties = ignoredUrlsProperties;
    }

    @Override
    protected void doFilterInternal(HttpServletRequest request,
                                    HttpServletResponse response,
                                    FilterChain chain) throws IOException, ServletException {

        // 忽略无需鉴权的路径，直接放行
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

        UsernamePasswordAuthenticationToken authentication = getAuthentication(jwt, response);
        if (authentication != null) {
            try {
                customAuthentication(request, response, authentication);
                SecurityContextHolder.getContext().setAuthentication(authentication);
            } catch (NoPermissionException e) {
                return;
            }
            chain.doFilter(request, response);
        } else {
            if (!response.isCommitted()) {
                ResponseUtil.output(response, 403, ResponseUtil.resultMap(false, 403, "登录已失效，请重新登录"));
            }
            return;
        }
    }

    /**
     * 自定义权限过滤
     *
     * @param request        请求
     * @param response       响应
     * @param authentication 用户信息
     */
    private void customAuthentication(HttpServletRequest request, HttpServletResponse response, UsernamePasswordAuthenticationToken authentication) throws NoPermissionException {
        AuthUser authUser = (AuthUser) authentication.getDetails();
        String requestUrl = request.getRequestURI();

        //如果不是超级管理员， 则鉴权
        if (Boolean.FALSE.equals(authUser.getIsSuper())) {
            String permissionCacheKey = CachePrefix.PERMISSION_LIST.getPrefix(UserEnums.MANAGER) + authUser.getId();
            //获取缓存中的权限
            Map<String, List<String>> permission =
                    (Map<String, List<String>>) cache.get(permissionCacheKey);
            if (permission == null || permission.isEmpty()) {
                permission = managerTokenGenerate.permissionList(this.menuService.findAllMenu(authUser.getId()));
                cache.put(permissionCacheKey, permission);
            }
            //获取数据(GET 请求)权限
            if (request.getMethod().equals(RequestMethod.GET.name())) {
                if (match(permission.get(PermissionEnum.SUPER.name()), requestUrl) ||
                        match(permission.get(PermissionEnum.QUERY.name()), requestUrl)) {
                } else {
                    ResponseUtil.output(response, ResponseUtil.resultMap(false, 400, "权限不足"));
                    log.error("当前请求路径：{},所拥有权限：{}", requestUrl, JSONUtil.toJsonStr(permission));
                    throw new NoPermissionException("权限不足");
                }
            } else {
                if (!match(permission.get(PermissionEnum.SUPER.name()), requestUrl)) {
                    ResponseUtil.output(response, ResponseUtil.resultMap(false, 400, "权限不足"));
                    log.error("当前请求路径：{},所拥有权限：{}", requestUrl, JSONUtil.toJsonStr(permission));
                    throw new NoPermissionException("权限不足");
                }
            }
        }
    }

    /**
     * 校验权限
     *
     * @param permissions 权限集合
     * @param url         请求地址
     * @return 是否拥有权限
     */
    boolean match(List<String> permissions, String url) {
        if (permissions == null || permissions.isEmpty()) {
            return false;
        }
        return PatternMatchUtils.simpleMatch(permissions.toArray(new String[0]), url);
    }

    /**
     * 获取token信息
     *
     * @param jwt      token信息
     * @param response 响应
     * @return 获取鉴权对象
     */
    private UsernamePasswordAuthenticationToken getAuthentication(String jwt, HttpServletResponse response) {

        try {
            Claims claims =
                    Jwts.parser()
                            .verifyWith(SecretKeyUtil.generalKeyByDecoders())
                            .build()
                            .parseSignedClaims(jwt)
                            .getPayload();
            //获取存储在claims中的用户信息
            String json = claims.get(SecurityEnum.USER_CONTEXT.getValue()).toString();
            AuthUser authUser = new Gson().fromJson(json, AuthUser.class);

            //校验redis中是否有权限
            if (cache.hasKey(CachePrefix.ACCESS_TOKEN.getPrefix(UserEnums.MANAGER, authUser.getId()) + jwt)) {
                List<GrantedAuthority> auths = new ArrayList<>();
                auths.add(new SimpleGrantedAuthority("ROLE_" + authUser.getRole().name()));
                UsernamePasswordAuthenticationToken authentication = new UsernamePasswordAuthenticationToken(authUser.getUsername(), null, auths);
                authentication.setDetails(authUser);
                return authentication;
            }
            ResponseUtil.output(response, 403, ResponseUtil.resultMap(false, 403, "登录已失效，请重新登录"));
            return null;
        } catch (ExpiredJwtException e) {
            log.debug("user token expired: {}", e.getMessage());
            ResponseUtil.output(response, 403, ResponseUtil.resultMap(false, 403, "登录已失效，请重新登录"));
        } catch (JwtException e) {
            log.warn("user token invalid: {}", e.getMessage());
            ResponseUtil.output(response, 403, ResponseUtil.resultMap(false, 403, "登录已失效，请重新登录"));
        } catch (Exception e) {
            log.error("other exception:", e);
        }
        return null;
    }
}

