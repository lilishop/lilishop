package cn.lili.common.aop.annotation;

import java.lang.annotation.*;

/**
 * 演示站点注解
 * <p>
 * PS 此注解需要用户登录之后才可以使用
 *
 * @author Bulbasaur
 * @since 2021/7/9 1:40 上午
 */
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
@Documented
@Inherited
public @interface DemoSite {


}
