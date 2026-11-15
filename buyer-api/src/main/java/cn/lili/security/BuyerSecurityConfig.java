package cn.lili.security;

import cn.lili.cache.Cache;
import cn.lili.common.properties.IgnoredUrlsProperties;
import cn.lili.common.security.CustomAccessDeniedHandler;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.annotation.Order;
import org.springframework.http.HttpMethod;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.config.annotation.authentication.configuration.AuthenticationConfiguration;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.web.cors.CorsConfigurationSource;

/**
 * 买家端 API 安全配置。
 */
@Configuration
@EnableWebSecurity
@EnableMethodSecurity(prePostEnabled = true)
@RequiredArgsConstructor
public class BuyerSecurityConfig {

    // 仅公开单个礼包的GET详情；领取路径多一层receive，仍须登录。
    static final String COUPON_PACKAGE_DETAIL_PATH = "/buyer/promotion/coupon/package/{id}";

    private final IgnoredUrlsProperties ignoredUrlsProperties;
    private final CustomAccessDeniedHandler accessDeniedHandler;
    private final Cache<String> cache;
    private final CorsConfigurationSource corsConfigurationSource;

    @Bean
    public AuthenticationManager authenticationManager(AuthenticationConfiguration cfg) throws Exception {
        return cfg.getAuthenticationManager();
    }

    @Bean
    @Order(10)
    public SecurityFilterChain buyerSecurityFilterChain(HttpSecurity http,
                                                        AuthenticationManager authManager) throws Exception {
        String[] ignored = ignoredUrlsProperties.getUrls().toArray(new String[0]);
        http.securityMatcher("/buyer/**")
            .authorizeHttpRequests(a -> a
                .requestMatchers(HttpMethod.GET, COUPON_PACKAGE_DETAIL_PATH).permitAll()
                .requestMatchers(ignored).permitAll()
                .anyRequest().authenticated())
            .headers(h -> h.frameOptions(f -> f.disable()))
            .logout(l -> l.permitAll())
            .cors(c -> c.configurationSource(corsConfigurationSource))
            .csrf(c -> c.disable())
            .sessionManagement(s -> s.sessionCreationPolicy(SessionCreationPolicy.STATELESS))
            .exceptionHandling(e -> e
                .accessDeniedHandler(accessDeniedHandler)
                .authenticationEntryPoint((req, res, ex) ->
                    cn.lili.common.utils.ResponseUtil.output(res, 403,
                        cn.lili.common.utils.ResponseUtil.resultMap(false, 403, "未登录或token失效"))))
            .formLogin(f -> f.disable())
            .httpBasic(b -> b.disable())
            .addFilter(new BuyerAuthenticationFilter(authManager, cache, ignoredUrlsProperties));
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
