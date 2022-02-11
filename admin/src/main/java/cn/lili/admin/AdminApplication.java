package cn.lili.admin;

import de.codecentric.boot.admin.server.config.AdminServerProperties;
import de.codecentric.boot.admin.server.config.EnableAdminServer;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.Customizer;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.web.authentication.SavedRequestAwareAuthenticationSuccessHandler;

import java.util.UUID;

/**
 * Admin
 *
 * @author Chopper
 * @since 2020/11/16 10:03 下午
 */
@Configuration
@EnableAutoConfiguration
@EnableAdminServer
public class AdminApplication {

    public static void main(String[] args) {
        SpringApplication.run(AdminApplication.class, args);
    }

    @Configuration
    public class SecuritySecureConfig extends WebSecurityConfigurerAdapter {

        private final AdminServerProperties adminServer;

        public SecuritySecureConfig(AdminServerProperties adminServer) {
            this.adminServer = adminServer;
        }

        @Override
        protected void configure(HttpSecurity http) throws Exception {
            SavedRequestAwareAuthenticationSuccessHandler successHandler = new SavedRequestAwareAuthenticationSuccessHandler();
            successHandler.setTargetUrlParameter("redirectTo");
            successHandler.setDefaultTargetUrl(this.adminServer.path("/"));
            http.authorizeRequests().antMatchers("/instances**").permitAll();
            http.authorizeRequests(
                    (authorizeRequests) -> authorizeRequests.antMatchers(this.adminServer.path("/assets/**")).permitAll() //授予公众对所有静态资产和登录页面的访问权限。
                            .antMatchers(this.adminServer.path("/login")).permitAll().anyRequest().authenticated() //其他所有请求都必须经过验证。
            ).formLogin(
                    (formLogin) -> formLogin.loginPage(this.adminServer.path("/login")).successHandler(successHandler).and() //配置登录和注销。
            ).logout((logout) -> logout.logoutUrl(this.adminServer.path("/logout"))).httpBasic(Customizer.withDefaults()) //启用HTTP基本支持。这是Spring Boot Admin Client注册所必需的。
                    .csrf().disable()
                    .rememberMe((rememberMe) -> rememberMe.key(UUID.randomUUID().toString()).tokenValiditySeconds(1209600));
        }

    }
}
