package cn.lili.cache.limit.annotation;


import cn.lili.cache.limit.enums.LimitTypeEnums;

import java.lang.annotation.*;

/**
 * 限流注解
 *
 * @author Chopper
 * @since 2018-02-05
 */
@Target({ElementType.METHOD, ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
public @interface LimitPoint {
    /**
     * 资源的名字 无实际意义，但是可以用于排除异常
     *
     * @return String
     */
    String name() default "";

    /**
     * 资源的key
     * <p>
     * 如果下方 limitType 值为自定义，那么全局限流参数来自于此
     * 如果limitType 为ip，则限流条件 为 key+ip
     *
     * @return String
     */
    String key() default "";

    /**
     * Key的prefix redis前缀，可选
     *
     * @return String
     */
    String prefix() default "";

    /**
     * 给定的时间段 单位秒
     *
     * @return int
     */
    int period() default 60;

    /**
     * 最多的访问限制次数
     *
     * @return int
     */
    int limit() default 10;

    /**
     * 类型  ip限制 还是自定义key值限制
     * 建议使用ip，自定义key属于全局限制，ip则是某节点设置，通常情况使用IP
     *
     * @return LimitType
     */
    LimitTypeEnums limitType() default LimitTypeEnums.IP;
}