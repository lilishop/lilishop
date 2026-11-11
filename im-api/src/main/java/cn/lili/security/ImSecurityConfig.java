package cn.lili.security;

import cn.lili.cache.Cache;
import cn.lili.common.properties.IgnoredUrlsProperties;
import cn.lili.common.security.CustomAccessDeniedHandler;
import cn.lili.common.utils.ResponseUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.annotation.Order;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.config.annotation.authentication.configuration.AuthenticationConfiguration;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.web.cors.CorsConfigurationSource;

/**
 * IM / 客服 API 安全配置
 *
 * @author Chopper
 * @since 2022-02-10
 */
@Configuration
@EnableWebSecurity
@EnableMethodSecurity(prePostEnabled = true)
@RequiredArgsConstructor
public class ImSecurityConfig {

    private final IgnoredUrlsProperties ignoredUrlsProperties;
    private final CustomAccessDeniedHandler accessDeniedHandler;
    private final CorsConfigurationSource corsConfigurationSource;
    private final Cache cache;

    @Bean
    public AuthenticationManager authenticationManager(AuthenticationConfiguration cfg) throws Exception {
        return cfg.getAuthenticationManager();
    }

    @Bean
    @Order(1)
    public SecurityFilterChain imSecurityFilterChain(HttpSecurity http,
                                                     AuthenticationManager authManager) throws Exception {
        String[] ignored = ignoredUrlsProperties != null && ignoredUrlsProperties.getUrls() != null
                ? ignoredUrlsProperties.getUrls().toArray(new String[0])
                : new String[0];

        http.securityMatcher("/im/**", "/seat/**", "/store/seat/**", "/manager/seat/**", "/lili/webSocket/**")
            .headers(h -> h.frameOptions(f -> f.disable()))
            .authorizeHttpRequests(a -> a
                .requestMatchers(ignored).permitAll()
                .requestMatchers("/lili/webSocket/**").permitAll()
                .requestMatchers("/seat/login/**").permitAll()
                .anyRequest().authenticated())
            .cors(c -> c.configurationSource(corsConfigurationSource))
            .csrf(csrf -> csrf.disable())
            .sessionManagement(s -> s.sessionCreationPolicy(SessionCreationPolicy.STATELESS))
            .exceptionHandling(e -> e
                .accessDeniedHandler(accessDeniedHandler)
                .authenticationEntryPoint((req, res, ex) ->
                    ResponseUtil.output(res, 403,
                        ResponseUtil.resultMap(false, 403, "未登录或token失效"))))
            .formLogin(f -> f.disable())
            .httpBasic(b -> b.disable())
            .addFilter(new ImAuthenticationFilter(authManager, cache, ignoredUrlsProperties));
        return http.build();
    }

    @Bean
    @Order(100)
    public SecurityFilterChain commonSecurityFilterChain(HttpSecurity http) throws Exception {
        String[] ignored = ignoredUrlsProperties != null && ignoredUrlsProperties.getUrls() != null
                ? ignoredUrlsProperties.getUrls().toArray(new String[0])
                : new String[0];
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
                    ResponseUtil.output(res, 403,
                        ResponseUtil.resultMap(false, 403, "未登录或token失效"))))
            .formLogin(f -> f.disable())
            .httpBasic(b -> b.disable());
        return http.build();
    }
}
