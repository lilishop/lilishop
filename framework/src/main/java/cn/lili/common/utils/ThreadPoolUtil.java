package cn.lili.common.utils;

import cn.hutool.core.thread.ThreadFactoryBuilder;
import lombok.extern.slf4j.Slf4j;

import java.util.concurrent.*;

/**
 * 通用业务线程池工具类
 * <p>
 * 遵循阿里开发规范：
 * 1. 彻底移除 Executors.newCachedThreadPool 等无界线程池工厂；
 * 2. 统一使用有界队列，避免突发高负载时产生 OOM；
 * 3. 采用标准化命名线程工厂（ThreadFactory），便于 Arthas、jstack、APM 监控排查；
 * 4. 统一 CallerRunsPolicy 拒绝策略（附带饱和警告日志），提供可靠反压能力；
 * 5. 注册 JVM 关闭钩子，保障服务优雅下线。
 *
 * @author Chopper, Antigravity
 */
@Slf4j
public class ThreadPoolUtil {

    /**
     * 核心线程数：根据 CPU 核心数自适应（最小为 8，最大为 CPU 核心数 * 2）
     */
    private static final int CPU_CORES = Runtime.getRuntime().availableProcessors();
    private static final int CORE_POOL_SIZE = Math.max(8, CPU_CORES);

    /**
     * 最大线程数
     */
    private static final int MAX_POOL_SIZE = Math.max(16, CPU_CORES * 2);

    /**
     * 空闲线程存活时间（秒）
     */
    private static final long KEEP_ALIVE_SECONDS = 60L;

    /**
     * 任务缓冲队列容量
     */
    private static final int QUEUE_CAPACITY = 1024;

    /**
     * 统一线程池实例（完全取代原有的 newCachedThreadPool 与混乱的双实例）
     */
    private static final ThreadPoolExecutor COMMON_POOL = new ThreadPoolExecutor(
            CORE_POOL_SIZE,
            MAX_POOL_SIZE,
            KEEP_ALIVE_SECONDS,
            TimeUnit.SECONDS,
            new LinkedBlockingQueue<>(QUEUE_CAPACITY),
            ThreadFactoryBuilder.create()
                    .setNamePrefix("lili-common-pool-")
                    .setDaemon(false)
                    .setUncaughtExceptionHandler((thread, throwable) ->
                            log.error("线程 [{}] 执行任务发生未捕获异常", thread.getName(), throwable)
                    )
                    .build(),
            new RejectedExecutionHandler() {
                private final RejectedExecutionHandler fallback = new ThreadPoolExecutor.CallerRunsPolicy();

                @Override
                public void rejectedExecution(Runnable r, ThreadPoolExecutor executor) {
                    log.warn("ThreadPoolUtil 达到饱和阈值！当前 poolSize={}, activeCount={}, queueSize={}. 触发 CallerRunsPolicy 回退主线程执行",
                            executor.getPoolSize(), executor.getActiveCount(), executor.getQueue().size());
                    fallback.rejectedExecution(r, executor);
                }
            }
    );

    static {
        // 预热核心线程
        COMMON_POOL.prestartAllCoreThreads();

        // 注册 JVM 关闭钩子进行优雅停机
        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            log.info("JVM 开始关闭，准备停止 ThreadPoolUtil 线程池...");
            shutdown();
        }, "lili-threadpool-shutdown-hook"));
    }

    private ThreadPoolUtil() {
    }

    /**
     * 提交异步无返回值任务
     *
     * @param runnable 任务
     */
    public static void execute(Runnable runnable) {
        COMMON_POOL.execute(runnable);
    }

    /**
     * 提交异步有返回值任务
     *
     * @param callable 任务
     * @param <T>      返回值类型
     * @return Future
     */
    public static <T> Future<T> submit(Callable<T> callable) {
        return COMMON_POOL.submit(callable);
    }

    /**
     * 提交异步任务
     *
     * @param runnable 任务
     * @return Future
     */
    public static Future<?> submit(Runnable runnable) {
        return COMMON_POOL.submit(runnable);
    }

    /**
     * 获取规范统一的线程池实例
     *
     * @return ThreadPoolExecutor
     */
    public static ThreadPoolExecutor getPool() {
        return COMMON_POOL;
    }

    /**
     * 获取线程池（兼容历史代码接口，统一指向规范线程池，彻底废弃 newCachedThreadPool）
     *
     * @return ThreadPoolExecutor
     */
    public static ThreadPoolExecutor getThreadPool() {
        return COMMON_POOL;
    }

    /**
     * 优雅关闭线程池
     */
    public static void shutdown() {
        if (!COMMON_POOL.isShutdown()) {
            COMMON_POOL.shutdown();
            try {
                if (!COMMON_POOL.awaitTermination(10, TimeUnit.SECONDS)) {
                    log.warn("ThreadPoolUtil 未能在 10 秒内优雅关闭，强制中断正在执行的任务...");
                    COMMON_POOL.shutdownNow();
                }
            } catch (InterruptedException e) {
                log.error("ThreadPoolUtil 关闭过程被中断", e);
                COMMON_POOL.shutdownNow();
                Thread.currentThread().interrupt();
            }
        }
    }
}
