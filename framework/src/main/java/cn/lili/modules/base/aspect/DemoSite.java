package cn.lili.modules.base.aspect;

import java.lang.annotation.*;

/**
 * 演示站点注解
 */
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
@Documented
@Inherited
public @interface DemoSite {


}
