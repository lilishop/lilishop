package cn.lili.modules.order.order.aop;

import java.lang.annotation.*;

/**
 * 售后日志AOP注解
 *
 * @author Chopper
 * @date 2020/11/17 7:22 下午
 */
@Target({ElementType.TYPE, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface AfterSaleLogPoint {

    /**
     * 日志名称
     *
     * @return
     */
    String description();

    /**
     * 售后SN
     *
     * @return
     */
    String sn();

}
