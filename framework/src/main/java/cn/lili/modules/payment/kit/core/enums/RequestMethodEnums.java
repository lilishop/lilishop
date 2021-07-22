package cn.lili.modules.payment.kit.core.enums;

/**
 * HTTP 请求的方法
 *
 * @author Chopper
 * @since 2021-01-25 15:10
 */
public enum RequestMethodEnums {
    /**
     * 上传实质是 post 请求
     */
    UPLOAD("POST"),
    /**
     * post 请求
     */
    POST("POST"),
    /**
     * get 请求
     */
    GET("GET"),
    /**
     * put 请求
     */
    PUT("PUT"),
    /**
     * delete 请求
     */
    DELETE("DELETE"),
    /**
     * options 请求
     */
    OPTIONS("OPTIONS"),
    /**
     * head 请求
     */
    HEAD("HEAD"),
    /**
     * trace 请求
     */
    TRACE("TRACE"),
    /**
     * connect 请求
     */
    CONNECT("CONNECT");

    private final String method;

    RequestMethodEnums(String method) {
        this.method = method;
    }

    @Override
    public String toString() {
        return this.method;
    }
}
