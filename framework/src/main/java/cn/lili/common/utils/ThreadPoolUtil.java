package cn.lili.common.utils;

import com.alibaba.ttl.TtlCallable;
import com.alibaba.ttl.TtlRunnable;

import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * 线程池工具类 (已修复并集成 TTL与优雅停机)
 *
 * @author Chopper (Refactored)
 */
public class ThreadPoolUtil {

    private static final int SIZE_CORE_POOL = 5;
    private static final int SIZE_MAX_POOL = 10;
    private static final long ALIVE_TIME = 2000;
    private static final BlockingQueue<Runnable> BQUEUE = new ArrayBlockingQueue<>(100);

    /**
     * 1. 增加自定义 ThreadFactory，方便排查线上问题时区分线程来源
     */
    private static final ThreadFactory THREAD_FACTORY = new ThreadFactory() {
        private final AtomicInteger threadNumber = new AtomicInteger(1);

        @Override
        public Thread newThread(Runnable r) {
            Thread t = new Thread(r, "lilishop-task-" + threadNumber.getAndIncrement());
            if (t.isDaemon()) {
                t.setDaemon(false);
            }
            if (t.getPriority() != Thread.NORM_PRIORITY) {
                t.setPriority(Thread.NORM_PRIORITY);
            }
            return t;
        }
    };

    /**
     * 初始化全局唯一线程池
     */
    private static final ThreadPoolExecutor threadPool = new ThreadPoolExecutor(
            SIZE_CORE_POOL, SIZE_MAX_POOL, ALIVE_TIME, TimeUnit.MILLISECONDS, BQUEUE,
            THREAD_FACTORY, // 使用自定义线程工厂
            new ThreadPoolExecutor.CallerRunsPolicy()
    );

    static {
        threadPool.prestartAllCoreThreads();

        // 2. 注册 JVM 关闭钩子，实现优雅停机
        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            System.out.println("JVM准备关闭，开始优雅停止 ThreadPoolUtil 线程池...");
            shutdown();
        }));
    }

    /**
     * 3. 私有化构造方法，防止外部滥用 new ThreadPoolUtil()
     */
    private ThreadPoolUtil() {
    }

    /**
     * 执行方法 (集成 TTL)
     */
    public static void execute(Runnable runnable) {
        threadPool.execute(TtlRunnable.get(runnable));
    }

    /**
     * 提交返回值 (集成 TTL)
     */
    public static <T> Future<T> submit(Callable<T> callable) {
        return threadPool.submit(TtlCallable.get(callable));
    }

    public static ThreadPoolExecutor getPool() {
        return threadPool;
    }

    /**
     * 4. 优雅关闭线程池的具体实现逻辑
     */
    private static void shutdown() {
        if (threadPool != null && !threadPool.isShutdown()) {
            // 拒绝接收新任务，但继续执行队列中的现有任务
            threadPool.shutdown();
            try {
                // 等待最多 60 秒让队列中的任务执行完毕
                if (!threadPool.awaitTermination(60, TimeUnit.SECONDS)) {
                    // 超时后强制关闭
                    threadPool.shutdownNow();
                    System.err.println("ThreadPoolUtil: 线程池任务执行超时，已强制关闭。");
                } else {
                    System.out.println("ThreadPoolUtil: 线程池已优雅关闭完成。");
                }
            } catch (InterruptedException e) {
                // 捕获到中断信号时强制关闭
                threadPool.shutdownNow();
                // 恢复中断状态
                Thread.currentThread().interrupt();
            }
        }
    }
}