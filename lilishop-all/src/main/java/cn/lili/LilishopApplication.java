package cn.lili;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Primary;
import org.springframework.core.task.TaskExecutor;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.web.socket.server.standard.ServerEndpointExporter;

/**
 * 单体合并入口：将 common-api / buyer-api / seller-api / manager-api / im-api / consumer
 * 合并为一个 Spring Boot 应用，运行在单端口。
 *
 * @author Chopper
 */
@SpringBootApplication(scanBasePackages = "cn.lili")
@EnableCaching
@EnableAsync
public class LilishopApplication {

    @Primary
    @Bean
    public TaskExecutor primaryTaskExecutor() {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        executor.setCorePoolSize(5);
        executor.setMaxPoolSize(50);
        executor.setQueueCapacity(50);
        executor.setThreadNamePrefix("lili-async-");
        executor.initialize();
        return executor;
    }

    @Bean
    public ServerEndpointExporter serverEndpointExporter() {
        return new ServerEndpointExporter();
    }

    public static void main(String[] args) {
        System.setProperty("es.set.netty.runtime.available.processors", "false");
        System.setProperty("rocketmq.client.logUseSlf4j", "true");
        SpringApplication.run(LilishopApplication.class, args);
    }
}
