package cn.lili.annotation;

import java.lang.annotation.*;
//MQ 消息路由标记注解
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface MQRouteTagHandler {


    //绑定的 MQ Tag 名称，例如 "MEMBER_REGISTER
    String value();
}
