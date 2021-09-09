package cn.lili.modules.system.aspect.annotation;

import java.lang.annotation.*;

/**
 * 演示站点注解
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
