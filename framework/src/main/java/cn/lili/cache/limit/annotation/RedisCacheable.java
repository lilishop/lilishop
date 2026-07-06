package cn.lili.cache.limit.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface RedisCacheable {
    String key();           // SpEL 表达式，比如 "#userId" 或 "#req.userId"
    long ttl() default 60L;   // 缓存时间（秒）
}

