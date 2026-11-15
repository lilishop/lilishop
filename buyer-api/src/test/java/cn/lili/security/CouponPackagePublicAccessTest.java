package cn.lili.security;

import cn.lili.cache.Cache;
import cn.lili.common.properties.IgnoredUrlsProperties;
import cn.lili.common.security.CustomAccessDeniedHandler;
import jakarta.servlet.Filter;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.context.web.WebAppConfiguration;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.context.WebApplicationContext;
import org.springframework.web.cors.CorsConfigurationSource;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

import static org.mockito.Mockito.mock;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * 使用真实买家安全过滤链验证礼包详情的匿名访问边界，不连接业务数据库或Redis。
 *
 * @author Bulbasaur
 * @since 2026-11-15
 */
@ExtendWith(SpringExtension.class)
@WebAppConfiguration
@ContextConfiguration(classes = CouponPackagePublicAccessTest.TestConfig.class)
class CouponPackagePublicAccessTest {

    @Autowired
    private WebApplicationContext context;

    private MockMvc mvc;

    /** 挂载实际安全过滤链，避免只测试URL字符串而遗漏认证过滤器行为。 */
    @BeforeEach
    void setUp() {
        mvc = MockMvcBuilders.webAppContextSetup(context)
                .addFilters(context.getBean("springSecurityFilterChain", Filter.class)).build();
    }

    /** 游客以及携带失效Token的访客均可浏览礼包详情。 */
    @Test
    void permitsPublicDetailWithOrWithoutToken() throws Exception {
        mvc.perform(get("/buyer/promotion/coupon/package/2121686668011044865")).andExpect(status().isOk());
        mvc.perform(get("/buyer/promotion/coupon/package/2121686668011044865")
                .header("accessToken", "expired-token")).andExpect(status().isOk());
    }

    /** 匿名领取、嵌套路径及其他会员接口不能借用详情放行规则。 */
    @Test
    void keepsReceiveAndOtherMemberEndpointsProtected() throws Exception {
        mvc.perform(post("/buyer/promotion/coupon/package/receive/2121686668011044865"))
                .andExpect(status().isForbidden());
        mvc.perform(get("/buyer/promotion/coupon/package/receive/2121686668011044865"))
                .andExpect(status().isForbidden());
        mvc.perform(get("/buyer/promotion/coupon/get/2121686668011044865"))
                .andExpect(status().isForbidden());
    }

    /** 公开规则仅限GET，不放行同一路径的写请求。 */
    @Test
    void rejectsAnonymousWriteToDetailPath() throws Exception {
        mvc.perform(post("/buyer/promotion/coupon/package/2121686668011044865"))
                .andExpect(status().isForbidden());
    }

    /**
     * 隔离外部依赖，只注册真实安全配置及用于观察过滤结果的端点。
     *
     * @author Bulbasaur
     * @since 2026-11-15
     */
    @Configuration
    @EnableWebMvc
    @Import({BuyerSecurityConfig.class, TestEndpoints.class})
    static class TestConfig {
        /** 提供空白名单，确保详情放行不依赖外部配置。 */
        @Bean
        IgnoredUrlsProperties ignoredUrlsProperties() { return new IgnoredUrlsProperties(); }

        /** 使用模拟缓存，测试不访问Redis。 */
        @Bean
        Cache<String> cache() { return mock(Cache.class); }

        /** 注册实际拒绝访问响应处理器。 */
        @Bean
        CustomAccessDeniedHandler accessDeniedHandler() { return new CustomAccessDeniedHandler(); }

        /** 提供CORS配置依赖，本测试不发送跨域请求。 */
        @Bean
        CorsConfigurationSource corsConfigurationSource() { return new UrlBasedCorsConfigurationSource(); }

        /** 提供认证管理器的用户服务依赖，不访问业务账号数据。 */
        @Bean
        UserDetailsService userDetailsService() { return mock(UserDetailsService.class); }
    }

    /**
     * 安全链测试端点，响应仅用于确认请求是否通过过滤链。
     *
     * @author Bulbasaur
     * @since 2026-11-15
     */
    @RestController
    static class TestEndpoints {
        /** 所有测试路径均有处理器，避免将404或405误当成权限拦截。 */
        @RequestMapping({"/buyer/promotion/coupon/package/{id}",
                "/buyer/promotion/coupon/package/receive/{id}", "/buyer/promotion/coupon/get/{id}"})
        String endpoint() { return "ok"; }
    }
}
