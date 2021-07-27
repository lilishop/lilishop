package cn.lili.modules.order.order.aop;

import java.lang.annotation.*;

/**
 * 订单日志AOP注解
 *
 * @author Chopper
 * @since 2020/11/17 7:22 下午
 */
@Target({ElementType.PARAMETER, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface OrderLogPoint {

    /**
     * 日志名称
     *
     * @return
     */
    String description();

    /**
     * 订单编号
     *
     * @return
     */
    String orderSn();

}
