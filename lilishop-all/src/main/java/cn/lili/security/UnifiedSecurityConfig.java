package cn.lili.security;

import cn.lili.cache.Cache;
import cn.lili.common.properties.IgnoredUrlsProperties;
import cn.lili.common.security.CustomAccessDeniedHandler;
import cn.lili.modules.member.service.ClerkService;
import cn.lili.modules.member.service.StoreMenuRoleService;
import cn.lili.modules.member.token.StoreTokenGenerate;
import cn.lili.modules.permission.service.MenuService;
import cn.lili.modules.system.token.ManagerTokenGenerate;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
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
 * 单体合并后的统一安全配置。
 * 通过 securityMatcher 按 URL 前缀分发到不同的 SecurityFilterChain：
 *   /buyer/**                                       → buyerChain（JWT 买家）
 *   /im/**, /seat/login, /store/seat/**, /manager/seat/** → imChain（permitAll，保留原 im-api 语义）
 *   /store/**                                       → storeChain（JWT 商家）
 *   /manager/**                                     → managerChain（JWT 平台管理员）
 *   /**                                             → commonChain（其余公共接口 permitAll）
 *
 * @author Chopper
 */
@Slf4j
@Configuration
@EnableWebSecurity
@EnableMethodSecurity(prePostEnabled = true)
@RequiredArgsConstructor
public class UnifiedSecurityConfig {

    private final IgnoredUrlsProperties ignoredUrlsProperties;
    private final CustomAccessDeniedHandler accessDeniedHandler;
    private final Cache<String> cache;
    private final CorsConfigurationSource corsConfigurationSource;
    private final StoreTokenGenerate storeTokenGenerate;
    private final StoreMenuRoleService storeMenuRoleService;
    private final ClerkService clerkService;
    private final MenuService menuService;
    private final ManagerTokenGenerate managerTokenGenerate;

    @Bean
    public AuthenticationManager authenticationManager(AuthenticationConfiguration cfg) throws Exception {
        return cfg.getAuthenticationManager();
    }

    /** IM / seat 类公共接口 — permitAll，最高优先级 */
    @Bean
    @Order(1)
    public SecurityFilterChain imSecurityFilterChain(HttpSecurity http) throws Exception {
        http.securityMatcher("/im/**", "/seat/login/**", "/store/seat/**", "/manager/seat/**", "/lili/webSocket/**")
            .headers(h -> h.frameOptions(f -> f.disable()))
            .authorizeHttpRequests(a -> a.anyRequest().permitAll())
            .cors(c -> c.configurationSource(corsConfigurationSource))
            .csrf(csrf -> csrf.disable())
            .formLogin(f -> f.disable())
            .httpBasic(b -> b.disable());
        return http.build();
    }

    /** 买家端 */
    @Bean
    @Order(10)
    public SecurityFilterChain buyerSecurityFilterChain(HttpSecurity http,
                                                        AuthenticationManager authManager) throws Exception {
        String[] ignored = ignoredUrlsProperties.getUrls().toArray(new String[0]);
        http.securityMatcher("/buyer/**")
            .authorizeHttpRequests(a -> a
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
            .addFilter(new BuyerAuthenticationFilter(authManager, cache));
        return http.build();
    }

    /** 商家端 */
    @Bean
    @Order(20)
    public SecurityFilterChain storeSecurityFilterChain(HttpSecurity http,
                                                        AuthenticationManager authManager) throws Exception {
        String[] ignored = ignoredUrlsProperties.getUrls().toArray(new String[0]);
        http.securityMatcher("/store/**")
            .authorizeHttpRequests(a -> a
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
            .addFilter(new StoreAuthenticationFilter(authManager, storeTokenGenerate,
                    storeMenuRoleService, clerkService, cache));
        return http.build();
    }

    /** 平台管理端 */
    @Bean
    @Order(30)
    public SecurityFilterChain managerSecurityFilterChain(HttpSecurity http,
                                                          AuthenticationManager authManager) throws Exception {
        http.securityMatcher("/manager/**")
            .headers(h -> h.frameOptions(f -> f.disable()))
            .authorizeHttpRequests(a -> {
                for (String url : ignoredUrlsProperties.getUrls()) {
                    a.requestMatchers(url).permitAll();
                }
                a.anyRequest().authenticated();
            })
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
            .addFilter(new ManagerAuthenticationFilter(authManager, menuService,
                    managerTokenGenerate, cache, ignoredUrlsProperties));
        return http.build();
    }

    /** 其余所有路径（/common/**、/druid/**、/swagger-ui/**、静态资源 等） — permitAll 兜底 */
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
