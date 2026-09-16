package cn.lili.common.exception;

import cn.lili.common.enums.ResultCode;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 全局业务异常类
 *
 * @author Chopper
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class ServiceException extends RuntimeException {

    private static final long serialVersionUID = 3447728300174142127L;


    /**
     * 异常消息
     */
    private String msg = ResultCode.ERROR.message();

    /**
     * 错误码
     */
    private ResultCode resultCode;

    public ServiceException(String msg) {
        super(msg);
        this.resultCode = ResultCode.ERROR;
        this.msg = msg;
    }

    public ServiceException() {
        this(ResultCode.ERROR);
    }

    public ServiceException(ResultCode resultCode) {
        super(resultCode.message());
        this.resultCode = resultCode;
        this.msg = resultCode.message();
    }

    public ServiceException(ResultCode resultCode, String message) {
        super(message);
        this.resultCode = resultCode;
        this.msg = message;
    }

}
