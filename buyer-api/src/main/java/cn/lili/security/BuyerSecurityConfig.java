package cn.lili.security;

import cn.lili.cache.Cache;
import cn.lili.common.security.CustomAccessDeniedHandler;
import cn.lili.common.properties.IgnoredUrlsProperties;
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
 * 统一安全配置 - 买家端
 * 认证机制：JWT 无状态
 * 异常处理：401 未认证、403 权限不足，统一 JSON 返回
 * CORS：统一由全局 CorsConfigurationSource 控制
 * 会话：STATELESS，禁用表单登录与 Basic
 *
 * @author Chopper
 * @version v4.0
 * @since 2020/11/14 16:20
 */

@Slf4j
@Configuration
@EnableWebSecurity
@EnableMethodSecurity(prePostEnabled = true)
public class BuyerSecurityConfig {

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

    /**
     * 配置安全过滤器链
     */
    @Bean
    public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
        // 配置不需要授权的URL
        String[] ignoredUrls = ignoredUrlsProperties.getUrls().toArray(new String[0]);
        
        http
            // 配置授权规则
            .authorizeHttpRequests(authz -> authz
                // 直播轮询接口允许匿名访问（高并发场景，登录用户可携带 token 获取个人数据）
                .requestMatchers("/buyer/live/room/polling/**").permitAll()
                // 配置的url不需要授权
                .requestMatchers(ignoredUrls).permitAll()
                // 任何其他请求都需要身份认证
                .anyRequest().authenticated()
            )
            // 禁止网页iframe
            .headers(headers -> headers.frameOptions(frameOptions -> frameOptions.disable()))
            // 配置登出
            .logout(logout -> logout.permitAll())
            // 允许跨域
            .cors(cors -> cors.configurationSource(corsConfigurationSource))
            // 关闭跨站请求防护
            .csrf(csrf -> csrf.disable())
            // 前后端分离采用JWT，不需要session
            .sessionManagement(session -> session.sessionCreationPolicy(SessionCreationPolicy.STATELESS))
            // 自定义权限拒绝处理类
            .exceptionHandling(exception -> exception
                .accessDeniedHandler(accessDeniedHandler)
                .authenticationEntryPoint((req, res, ex) ->
                    cn.lili.common.utils.ResponseUtil.output(res, 403,
                        cn.lili.common.utils.ResponseUtil.resultMap(false, 403, "未登录或token失效"))
                )
            )
            // 禁用表单与Basic认证
            .formLogin(form -> form.disable())
            .httpBasic(basic -> basic.disable())
            // 添加JWT认证过滤器
            .addFilter(new BuyerAuthenticationFilter(authenticationManager(authenticationConfiguration()), cache, ignoredUrlsProperties));

        return http.build();
    }

    /**
     * 获取AuthenticationManager Bean
     */
    @Bean
    public AuthenticationManager authenticationManager(AuthenticationConfiguration authenticationConfiguration) throws Exception {
        return authenticationConfiguration.getAuthenticationManager();
    }

    /**
     * 注入AuthenticationConfiguration
     */
    @Autowired
    private AuthenticationConfiguration authenticationConfiguration;

    /**
     * 获取AuthenticationConfiguration Bean
     */
    @Bean
    public AuthenticationConfiguration authenticationConfiguration() {
        return this.authenticationConfiguration;
    }
}
