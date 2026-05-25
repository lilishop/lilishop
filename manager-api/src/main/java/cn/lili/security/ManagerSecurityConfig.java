package cn.lili.security;

import cn.lili.cache.Cache;
import cn.lili.common.properties.IgnoredUrlsProperties;
import cn.lili.common.security.CustomAccessDeniedHandler;
import cn.lili.modules.permission.service.MenuService;
import cn.lili.modules.system.token.ManagerTokenGenerate;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.config.annotation.authentication.configuration.AuthenticationConfiguration;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.web.cors.CorsConfigurationSource;

/**
 * 统一安全配置 - 管理端
 * 认证机制：JWT 无状态，仅匹配 /manager/**
 * 异常处理：401 未认证、403 权限不足，统一 JSON 返回
 * CORS：统一由全局 CorsConfigurationSource 控制
 * 会话：STATELESS，禁用表单登录与 Basic
 *
 * @author Chopper
 * @since 2020/11/14 16:20
 */
@Slf4j
@Configuration
@EnableWebSecurity
@EnableMethodSecurity(prePostEnabled = true)
public class ManagerSecurityConfig {

    @Autowired
    public MenuService menuService;
    /**
     * 忽略验权配置
     */
    @Autowired
    private IgnoredUrlsProperties ignoredUrlsProperties;
    /**
     * spring security -》 权限不足处理
     */
    @Autowired
    private CustomAccessDeniedHandler accessDeniedHandler;
    @Autowired
    private Cache<String> cache;
    @Autowired
    private CorsConfigurationSource corsConfigurationSource;
    @Autowired
    private ManagerTokenGenerate managerTokenGenerate;
    @Autowired
    private AuthenticationConfiguration authenticationConfiguration;

    @Bean
    public SecurityFilterChain securityFilterChain(HttpSecurity http, AuthenticationManager authenticationManager) throws Exception {
        // 禁止网页 iframe
        http.headers(h -> h.frameOptions(f -> f.disable()))
            .authorizeHttpRequests(auth -> {
                for (String url : ignoredUrlsProperties.getUrls()) {
                    auth.requestMatchers(url).permitAll();
                }
                auth.anyRequest().authenticated();
            })
            // 配置登出
            .logout(logout -> logout.permitAll())
            // 允许跨域
            .cors(c -> c.configurationSource(corsConfigurationSource))
            // 关闭 CSRF（前后端分离）
            .csrf(csrf -> csrf.disable())
            // 前后端分离采用 JWT 不需要 session
            .sessionManagement(sm -> sm.sessionCreationPolicy(SessionCreationPolicy.STATELESS))
            // 自定义权限拒绝处理，改为返回 JSON 而非重定向
            .exceptionHandling(eh -> eh
                    .accessDeniedHandler(accessDeniedHandler)
                    .authenticationEntryPoint((req, res, ex) ->
                            cn.lili.common.utils.ResponseUtil.output(res, 403,
                                    cn.lili.common.utils.ResponseUtil.resultMap(false, 403, "未登录或token失效"))
                    )
            )
            .formLogin(form -> form.disable())
            .httpBasic(basic -> basic.disable());

        http.addFilter(new ManagerAuthenticationFilter(
            authenticationManager,
            menuService,
            managerTokenGenerate,
            cache,
            ignoredUrlsProperties
        ));

        return http.build();
    }

    /**
     * 显式暴露 AuthenticationManager，避免自动配置变更导致 JWT 过滤器注入失败
     */
    @Bean
    public AuthenticationManager authenticationManager() throws Exception {
        return authenticationConfiguration.getAuthenticationManager();
    }
}
