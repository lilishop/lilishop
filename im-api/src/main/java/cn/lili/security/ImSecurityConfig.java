package cn.lili.security;

import cn.lili.common.properties.IgnoredUrlsProperties;
import cn.lili.common.security.CustomAccessDeniedHandler;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.annotation.Order;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.web.cors.CorsConfigurationSource;

/**
 * IM / 客服 API 安全配置（WebSocket 与 seat 接口 permitAll）。
 */
@Configuration
@EnableWebSecurity
@EnableMethodSecurity(prePostEnabled = true)
@RequiredArgsConstructor
public class ImSecurityConfig {

    private final IgnoredUrlsProperties ignoredUrlsProperties;
    private final CustomAccessDeniedHandler accessDeniedHandler;
    private final CorsConfigurationSource corsConfigurationSource;

    @Bean
    @Order(1)
    public SecurityFilterChain imSecurityFilterChain(HttpSecurity http) throws Exception {
        http.securityMatcher("/im/**", "/seat/**", "/store/seat/**", "/manager/seat/**", "/lili/webSocket/**")
            .headers(h -> h.frameOptions(f -> f.disable()))
            .authorizeHttpRequests(a -> a.anyRequest().permitAll())
            .cors(c -> c.configurationSource(corsConfigurationSource))
            .csrf(csrf -> csrf.disable())
            .formLogin(f -> f.disable())
            .httpBasic(b -> b.disable());
        return http.build();
    }

    @Bean
    @Order(100)
    public SecurityFilterChain commonSecurityFilterChain(HttpSecurity http) throws Exception {
        String[] ignored = ignoredUrlsProperties.getUrls().toArray(new String[0]);
        http
            .headers(h -> h.frameOptions(f -> f.disable()))
            .authorizeHttpRequests(a -> a
                .requestMatchers(ignored).permitAll()
                .anyRequest().permitAll())
            .cors(c -> c.configurationSource(corsConfigurationSource))
            .csrf(c -> c.disable())
            .exceptionHandling(e -> e
                .accessDeniedHandler(accessDeniedHandler)
                .authenticationEntryPoint((req, res, ex) ->
                    cn.lili.common.utils.ResponseUtil.output(res, 403,
                        cn.lili.common.utils.ResponseUtil.resultMap(false, 403, "未登录或token失效"))))
            .formLogin(f -> f.disable())
            .httpBasic(b -> b.disable());
        return http.build();
    }
}
