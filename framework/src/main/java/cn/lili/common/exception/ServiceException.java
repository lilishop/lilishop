package cn.lili.common.exception;

import cn.lili.common.enums.ResultCode;
import lombok.Data;

/**
 * @author Chopper
 */
@Data
public class ServiceException extends RuntimeException {

    private String msg;

    private ResultCode resultCode;

    public ServiceException(String msg) {
        super(msg);
        this.msg = msg;
    }

    public ServiceException() {
        super("网络错误，请稍后重试！");
        this.msg = "网络错误，请稍后重试！";
    }

    public ServiceException(ResultCode resultCode) {
        this.resultCode = resultCode;
    }

}
