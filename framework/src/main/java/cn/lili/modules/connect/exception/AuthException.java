package cn.lili.modules.connect.exception;

import cn.lili.modules.connect.config.ConnectAuth;
import cn.lili.modules.connect.entity.enums.AuthResponseStatus;

/**
 * JustAuth通用异常类
 *
 * @author yadong.zhang (yadong.zhang0415(a)gmail.com)
 * @since 1.8
 */
public class AuthException extends RuntimeException {

    private int errorCode;
    private String errorMsg;

    public AuthException(String errorMsg) {
        this(AuthResponseStatus.FAILURE.getCode(), errorMsg);
    }

    public AuthException(String errorMsg, ConnectAuth source) {
        this(AuthResponseStatus.FAILURE.getCode(), errorMsg, source);
    }

    public AuthException(int errorCode, String errorMsg) {
        super(errorMsg);
        this.errorCode = errorCode;
        this.errorMsg = errorMsg;
    }

    public AuthException(AuthResponseStatus status) {
        this(status.getCode(), status.getMsg());
    }

    public AuthException(int errorCode, String errorMsg, ConnectAuth source) {
        this(errorCode, String.format("%s [%s]", errorMsg, source.getName()));
    }

    public AuthException(AuthResponseStatus status, ConnectAuth source) {
        this(status.getCode(), status.getMsg(), source);
    }

    public AuthException(String message, Throwable cause) {
        super(message, cause);
    }

    public AuthException(Throwable cause) {
        super(cause);
    }

    public int getErrorCode() {
        return errorCode;
    }

    public String getErrorMsg() {
        return errorMsg;
    }
}
