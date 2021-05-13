package cn.lili.modules.member.entity.aop.annotation;


import java.lang.annotation.*;

/**
 * 会员积分操作aop
 *
 * @author pikachu
 * @date 2020/11/17 7:22 下午
 */
@Target({ElementType.PARAMETER, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface PointLogPoint {

}
