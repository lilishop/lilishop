package cn.lili.modules.base.aspect;

import java.lang.annotation.*;

/**
 * 演示站点注解
 *
 * @author Bulbasaur
 * @date: 2021/7/9 1:40 上午
 */
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
@Documented
@Inherited
public @interface DemoSite {


}
