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
            } catch (RetryException ex) {
                log.info("Operation failed, retries remaining: {}", retryCount);
                retryCount--;
                if (retryCount < 0) {
                    successful = true;
                    log.error(ex.getMessage());
                }
                if (waitSeconds > 0 && !successful) {
                    log.info("Waiting for {} second(s) before next retry", waitSeconds);
                    Thread.sleep(waitSeconds * 1000L);
                }
            }
        } while (!successful);

        return response;
    }

}
