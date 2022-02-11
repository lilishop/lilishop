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
 * 运营后台 API
 *
 * @author Chopper
 * @since 2020/11/16 10:03 下午
 */
@SpringBootApplication
@EnableCaching
@EnableAsync
public class ManagerApiApplication {

    @Primary
    @Bean
    public TaskExecutor primaryTask() {
        return new ThreadPoolTaskExecutor();
    }

    public static void main(String[] args) {
        System.setProperty("es.set.netty.runtime.available.processors", "false");
        SpringApplication.run(ManagerApiApplication.class, args);
    }

}
