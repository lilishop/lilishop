package cn.lili;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Primary;
import org.springframework.core.task.TaskExecutor;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

/**
 * 买家API
 *
 * @author Chopper
 * @since 2020/11/16 10:03 下午
 */
@SpringBootApplication
@EnableCaching
@EnableAsync
public class BuyerApiApplication {


    @Primary
    @Bean
    public TaskExecutor primaryTaskExecutor() {
        return new ThreadPoolTaskExecutor();
    }

    public static void main(String[] args) {
        System.setProperty("es.set.netty.runtime.available.processors", "false");
        System.setProperty("rocketmq.client.logUseSlf4j","true");
        SpringApplication.run(BuyerApiApplication.class, args);
    }
}
