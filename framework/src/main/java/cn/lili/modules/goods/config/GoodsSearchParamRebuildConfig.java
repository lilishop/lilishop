package cn.lili.modules.goods.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

import java.util.concurrent.ThreadPoolExecutor;

/**
 * 商品参数倒排全量重建专用执行器：单线程、队列容量 1、拒绝丢弃。
 * 锁必须在 worker 线程内获取和释放（Redisson RLock 线程绑定）。
 *
 * @author Mike
 * @date 2026-09-03
 */
@Configuration
public class GoodsSearchParamRebuildConfig {

    public static final String EXECUTOR_BEAN_NAME = "goodsSearchParamRebuildExecutor";

    @Bean(name = EXECUTOR_BEAN_NAME)
    public ThreadPoolTaskExecutor goodsSearchParamRebuildExecutor() {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        executor.setCorePoolSize(1);
        executor.setMaxPoolSize(1);
        executor.setQueueCapacity(1);
        executor.setThreadNamePrefix("goods-search-param-rebuild-");
        executor.setRejectedExecutionHandler(new ThreadPoolExecutor.DiscardPolicy());
        executor.initialize();
        return executor;
    }
}
