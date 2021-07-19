package cn.lili.cache.limit.enums;


/**
 * redis 限流类型
 *
 * @author Chopper
 * @since 2018年2月02日 下午4:58:52
 */

public enum LimitTypeEnums {
    /**
     * 自定义key(即全局限流)
     */
    CUSTOMER,
    /**
     * 根据请求者IP（IP限流）
     */
    IP
}
