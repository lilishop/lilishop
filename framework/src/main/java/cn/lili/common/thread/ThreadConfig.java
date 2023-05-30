package cn.lili.common.thread;

import cn.lili.common.properties.ThreadProperties;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.AsyncConfigurer;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

import java.util.concurrent.Executor;

/**
 * 多线程配置
 *
 * @author Chopper
 * @version v1.0
 * @since
 * 2020-03-12 10:50 上午
 */
@Configuration
public class ThreadConfig implements AsyncConfigurer {


    @Autowired
    private ThreadProperties threadProperties;


    @Override
    public Executor getAsyncExecutor() {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
//       核心线程数，默认为5
        executor.setCorePoolSize(threadProperties.getCorePoolSize());
//       最大线程数，默认为10
        executor.setMaxPoolSize(threadProperties.getMaxPoolSize());
//     队列最大长度，一般需要设置值为足够大
        executor.setQueueCapacity(threadProperties.getQueueCapacity());
//       线程池维护线程所允许的空闲时间，默认为60s
        executor.setKeepAliveSeconds(threadProperties.getKeepAliveSeconds());
//       允许超时关闭
        executor.setAllowCoreThreadTimeOut(threadProperties.getAllowCoreThreadTimeOut());
        executor.initialize();
        return executor;
    }
}
