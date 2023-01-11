package cn.lili.modules.im.config;


import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * WebSocketConfigurator
 *
 * @author Chopper
 * @version v1.0
 * 2021-12-31 11:53
 */
@ConditionalOnWebApplication
@Configuration
public class WebSocketConfigurator {

    @Bean
    public CustomSpringConfigurator customSpringConfigurator() {
        // This is just to get context
        return new CustomSpringConfigurator();
    }
}
