package cn.lili.common.thread;

import cn.lili.common.properties.ThreadProperties;
import lombok.extern.slf4j.Slf4j;
import org.springframework.aop.interceptor.AsyncUncaughtExceptionHandler;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.AsyncConfigurer;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

import java.util.concurrent.Executor;
import java.util.concurrent.ThreadPoolExecutor;

/**
 * 多线程配置（Spring @Async 异步任务执行器）
 *
 * @author Chopper, Antigravity
 * @version v1.0
 * @since 2020-03-12 10:50 上午
 */
@Slf4j
@Configuration
public class ThreadConfig implements AsyncConfigurer {

    @Autowired
    private ThreadProperties threadProperties;

    @Override
    public Executor getAsyncExecutor() {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        // 核心线程数
        executor.setCorePoolSize(threadProperties.getCorePoolSize());
        // 最大线程数
        executor.setMaxPoolSize(threadProperties.getMaxPoolSize());
        // 队列最大长度
        executor.setQueueCapacity(threadProperties.getQueueCapacity());
        // 线程池维护线程所允许的空闲时间
        executor.setKeepAliveSeconds(threadProperties.getKeepAliveSeconds());
        // 允许超时关闭
        executor.setAllowCoreThreadTimeOut(threadProperties.getAllowCoreThreadTimeOut());
        // 标准线程名称前缀，便于监控与排查
        executor.setThreadNamePrefix("lili-async-");
        // 拒绝策略：主线程回退执行，保证任务不丢失并形成天然反压
        executor.setRejectedExecutionHandler(new ThreadPoolExecutor.CallerRunsPolicy());
        // 优雅停机配置
        executor.setWaitForTasksToCompleteOnShutdown(true);
        executor.setAwaitTerminationSeconds(30);
        executor.initialize();
        return executor;
    }

    @Override
    public AsyncUncaughtExceptionHandler getAsyncUncaughtExceptionHandler() {
        return (throwable, method, objects) ->
                log.error("Spring @Async 异步方法 [{}] 执行异常", method.getName(), throwable);
    }
}
