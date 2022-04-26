package cn.lili.common.exception;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 如需异常重试，则抛出此异常
 *
 * @author paulG
 * @since 2022/4/26
 **/
@EqualsAndHashCode(callSuper = true)
@Data
public class RetryException extends RuntimeException {

    private static final long serialVersionUID = 7886918292771470846L;

    public RetryException(String message) {
        super(message);
    }
}
