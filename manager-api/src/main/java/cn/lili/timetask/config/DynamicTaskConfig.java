package cn.lili.timetask.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.concurrent.ThreadPoolTaskScheduler;

/**
 * 动态定时任务调度线程池配置
 *
 * @author Bulbasaur
 * @since 2026-10-29
 */
@Configuration
@EnableScheduling
public class DynamicTaskConfig {

    @Bean(name = "dynamicThreadPoolTaskScheduler")
    public ThreadPoolTaskScheduler dynamicThreadPoolTaskScheduler() {
        ThreadPoolTaskScheduler scheduler = new ThreadPoolTaskScheduler();
        scheduler.setPoolSize(10);
        scheduler.setThreadNamePrefix("dynamic-task-");
        scheduler.setWaitForTasksToCompleteOnShutdown(true);
        scheduler.setAwaitTerminationSeconds(30);
        scheduler.initialize();
        return scheduler;
    }
}
