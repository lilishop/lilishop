package cn.lili.config.interceptor;

import cn.lili.config.properties.IgnoredUrlsProperties;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.InterceptorRegistration;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.ResourceHandlerRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

/**
 * 过滤路径
 *
 * @author Chopper
 */
@Configuration
public class UrlConfiguration implements WebMvcConfigurer {
    @Autowired
    private IgnoredUrlsProperties ignoredUrlsProperties;
    @Autowired
    private RequestInterceptorAdapter requestInterceptorAdapter;

    @Override
    public void addInterceptors(InterceptorRegistry registry) {
        //注册拦截器
        InterceptorRegistration ir = registry.addInterceptor(requestInterceptorAdapter);
        //配置拦截的路径
        ir.addPathPatterns("/**");
        //配置不拦截的路径
        ir.excludePathPatterns(ignoredUrlsProperties.getUrls());
    }

    /**
     * 开放资源 这里配置swagger可以在前端访问
     * @param registry
     */
    @Override
    public void addResourceHandlers(ResourceHandlerRegistry registry) {
        registry.addResourceHandler("/statics/**").addResourceLocations("classpath:/statics/");
        registry.addResourceHandler("/webjars/**").addResourceLocations("classpath:/META-INF/resources/webjars/");
        //解决 SWAGGER 404报错
        registry.addResourceHandler("/swagger-ui.html").addResourceLocations("classpath:/META-INF/resources/");
    }

}
