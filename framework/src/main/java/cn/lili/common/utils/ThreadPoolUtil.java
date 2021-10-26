package cn.lili.common.utils;

import java.util.concurrent.*;

/**
 * @author Chopper
 */
public class ThreadPoolUtil {

    /**
     * 核心线程数，会一直存活，即使没有任务，线程池也会维护线程的最少数量
     */
    private static final int SIZE_CORE_POOL = 5;
    /**
     * 线程池维护线程的最大数量
     */
    private static final int SIZE_MAX_POOL = 10;
    /**
     * 线程池维护线程所允许的空闲时间
     */
    private static final long ALIVE_TIME = 2000;
    /**
     * 线程缓冲队列
     */
    private static final BlockingQueue<Runnable> BQUEUE = new ArrayBlockingQueue<Runnable>(100);
    private static final ThreadPoolExecutor POOL = new ThreadPoolExecutor(SIZE_CORE_POOL, SIZE_MAX_POOL, ALIVE_TIME, TimeUnit.MILLISECONDS, BQUEUE, new ThreadPoolExecutor.CallerRunsPolicy());
    /**
     * volatile禁止指令重排
     */
    public static volatile ThreadPoolExecutor threadPool;

    static {
        POOL.prestartAllCoreThreads();
    }

    /**
     * 无返回值直接执行, 管他娘的
     *
     * @param runnable
     */
    public static void execute(Runnable runnable) {
        getThreadPool().execute(runnable);
    }

    /**
     * 返回值直接执行, 管他娘的
     *
     * @param callable
     */
    public static <T> Future<T> submit(Callable<T> callable) {
        return getThreadPool().submit(callable);
    }

    /**
     * DCL获取线程池
     *
     * @return 线程池对象
     */
    public static ThreadPoolExecutor getThreadPool() {
        if (threadPool != null) {
            return threadPool;
        }
        synchronized (ThreadPoolUtil.class) {
            if (threadPool == null) {
                threadPool = (ThreadPoolExecutor) Executors.newCachedThreadPool();
            }
        }
        return threadPool;
    }

    public static ThreadPoolExecutor getPool() {
        return POOL;
    }

    public static void main(String[] args) {
        System.out.println(POOL.getPoolSize());
    }
}
