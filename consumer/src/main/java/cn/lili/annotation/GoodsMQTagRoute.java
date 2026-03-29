package cn.lili.annotation;

import cn.lili.rocketmq.tags.GoodsTagsEnum;

import java.lang.annotation.*;

@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface GoodsMQTagRoute {

    GoodsTagsEnum value();
}
