package cn.lili.modules.goods.config;

import org.junit.jupiter.api.Test;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

import java.util.concurrent.ThreadPoolExecutor;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 全量重建专用执行器：单线程、队列容量 1、拒绝丢弃。
 *
 * @author Mike
 * @date 2026-09-03
 */
class GoodsSearchParamRebuildConfigTest {

    @Test
    void executorIsSingleThreadWithQueueOneAndDiscardPolicy() {
        GoodsSearchParamRebuildConfig config = new GoodsSearchParamRebuildConfig();
        ThreadPoolTaskExecutor executor = config.goodsSearchParamRebuildExecutor();
        try {
            assertThat(executor.getCorePoolSize()).isEqualTo(1);
            assertThat(executor.getMaxPoolSize()).isEqualTo(1);
            assertThat(executor.getThreadNamePrefix()).isEqualTo("goods-search-param-rebuild-");
            ThreadPoolExecutor raw = executor.getThreadPoolExecutor();
            assertThat(raw.getQueue().remainingCapacity()).isEqualTo(1);
            assertThat(raw.getRejectedExecutionHandler()).isInstanceOf(ThreadPoolExecutor.DiscardPolicy.class);
        } finally {
            executor.shutdown();
        }
    }
}
