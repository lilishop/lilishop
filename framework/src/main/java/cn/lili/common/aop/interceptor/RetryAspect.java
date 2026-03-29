package cn.lili.common.aop.interceptor;

import cn.lili.common.aop.annotation.RetryOperation;
import cn.lili.common.exception.RetryException;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.stereotype.Component;

/**
 * @author paulG
 * @since 2022/4/26
 **/
@Aspect
@Component
@Slf4j
public class RetryAspect {

    // 不建议使用这个工具，不如直接使用spring 自带的，然后自己指定线程池，监控线程

    @Around(value = "@annotation(retryOperation)")
    public Object retryOperation(ProceedingJoinPoint joinPoint, RetryOperation retryOperation) throws Throwable {

        Object response = null;
        int retryCount = retryOperation.retryCount();
        int waitSeconds = retryOperation.waitSeconds();
        boolean successful = false;

        do {
            try {
                response = joinPoint.proceed();
                successful = true;
                //存在的问题 1，异常类只捕获这个 RetryException 2.没有把异常跑出去，排查问题麻烦
            } catch (RetryException ex) {
                log.info("Operation failed, retries remaining: {}", retryCount);
                retryCount--;
                if (retryCount < 0) {
                    successful = true;
                    log.error(ex.getMessage());
                }
                if (waitSeconds > 0 && !successful) {
                    log.info("Waiting for {} second(s) before next retry", waitSeconds);
                    Thread.sleep(waitSeconds * 1000L);//居然是睡固定时间，实际应该搞成 退避策略（Backoff）：
                    //支持指数退避（Exponential Backoff）。第一次失败等 1 秒，第二次失败等 2 秒，第三次等 4 秒。给下游服务喘息恢复的时间
                }
            }
        } while (!successful);

        return response;
    }

}
