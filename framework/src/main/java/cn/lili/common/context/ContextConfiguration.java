package cn.lili.common.context;

import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

/**
 * 过滤路径
 * @author Chopper
 */
@Configuration
public class ContextConfiguration implements WebMvcConfigurer {

    @Override
    public void addInterceptors(InterceptorRegistry registry) {
        registry.addInterceptor(new ThreadContextHolderInterceptorAdapter()).addPathPatterns("/**");
    }

}
