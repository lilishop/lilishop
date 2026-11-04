package cn.lili.common.utils;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import static org.assertj.core.api.Assertions.assertThat;

class ThreadPoolUtilTest {

    /**
     * 验证两个线程池获取入口返回同一实例。
     */
    @Test
    @DisplayName("getPool 和 getThreadPool 必须返回同一个单例实例，彻底废弃双实例与 newCachedThreadPool")
    void getPoolAndGetThreadPoolReturnSameInstance() {
        ThreadPoolExecutor pool1 = ThreadPoolUtil.getPool();
        ThreadPoolExecutor pool2 = ThreadPoolUtil.getThreadPool();

        assertThat(pool1).isNotNull();
        assertThat(pool2).isNotNull();
        assertThat(pool1).isSameAs(pool2);
    }

    /**
     * 验证线程池使用有界线程数及任务队列。
     */
    @Test
    @DisplayName("线程池核心参数必须有界，不得为 Integer.MAX_VALUE")
    void poolParametersMustBeBounded() {
        ThreadPoolExecutor pool = ThreadPoolUtil.getPool();

        assertThat(pool.getMaximumPoolSize()).isLessThan(Integer.MAX_VALUE);
        assertThat(pool.getCorePoolSize()).isGreaterThanOrEqualTo(8);
        assertThat(pool.getMaximumPoolSize()).isGreaterThanOrEqualTo(pool.getCorePoolSize());
        assertThat(pool.getQueue()).isInstanceOf(LinkedBlockingQueue.class);
        assertThat(pool.getQueue().remainingCapacity() + pool.getQueue().size()).isEqualTo(1024);
    }

    /**
     * 验证执行任务的线程名称包含约定前缀。
     */
    @Test
    @DisplayName("execute 执行的任务线程名称必须遵循 lili-common-pool- 前缀规范")
    void executeRunsWithNamedThreadPrefix() throws Exception {
        CountDownLatch latch = new CountDownLatch(1);
        AtomicReference<String> threadNameRef = new AtomicReference<>();

        ThreadPoolUtil.execute(() -> {
            threadNameRef.set(Thread.currentThread().getName());
            latch.countDown();
        });

        boolean finished = latch.await(3, TimeUnit.SECONDS);
        assertThat(finished).isTrue();
        assertThat(threadNameRef.get()).startsWith("lili-common-pool-");
    }

    /**
     * 验证提交有返回值的任务可通过Future获取结果。
     */
    @Test
    @DisplayName("submit(Callable) 能够正确提交并获取返回值")
    void submitCallableReturnsFutureResult() throws Exception {
        Future<String> future = ThreadPoolUtil.submit(() -> "lili-task-result");

        String result = future.get(3, TimeUnit.SECONDS);
        assertThat(result).isEqualTo("lili-task-result");
    }

    /**
     * 验证提交无返回值的任务能够正常完成。
     */
    @Test
    @DisplayName("submit(Runnable) 能够正确提交并完成")
    void submitRunnableCompletesSuccessfully() throws Exception {
        AtomicBoolean flag = new AtomicBoolean(false);
        Future<?> future = ThreadPoolUtil.submit(() -> flag.set(true));

        future.get(3, TimeUnit.SECONDS);
        assertThat(flag.get()).isTrue();
    }
}
