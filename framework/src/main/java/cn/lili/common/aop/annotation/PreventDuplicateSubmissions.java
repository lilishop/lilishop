package cn.lili.common.aop.annotation;

import java.lang.annotation.*;

/**
 * 防止重复提交注解
 *
 * @author liushuai(liushuai711 @ gmail.com)
 * @version v4.0
 * @Description:
 * @since 2022/1/25 09:17
 */
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
@Documented
@Inherited
public @interface PreventDuplicateSubmissions {


    /**
     * 过期时间
     */
    long expire() default 3;
}
