package cn.lili.modules.statistics.aop;

import cn.lili.modules.statistics.aop.enums.PageViewEnum;

import java.lang.annotation.*;

/**
 * 埋点统计
 *
 * @author Bulbasaur
 * @since 2021/7/9 1:47 上午
 */
@Target({ElementType.PARAMETER, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface PageViewPoint {

    /**
     * 描述
     */
    PageViewEnum type();

    /**
     * 如：商品id，店铺id
     * 字段类型为string ，支持 spel语法，也可以填写
     */
    String id();
}