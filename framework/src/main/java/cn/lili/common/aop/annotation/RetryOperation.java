package cn.lili.common.aop.annotation;

import java.lang.annotation.*;

/**
 * 异常重试注解
 *
 * @author paulG
 * @since 2022/4/26
 **/
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
@Documented
@Inherited
public @interface RetryOperation {
    /**
     * 重试次数
     */
    int retryCount() default 3;

    /**
     * 重试间隔
     */
    int waitSeconds() default 10;
}
