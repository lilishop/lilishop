package cn.lili.common.exception;

import cn.lili.common.enums.ResultCode;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class ServiceExceptionTest {

    @Test
    void resultCodeConstructorUsesResultCodeMessageInsteadOfDefaultBusyText() {
        ServiceException exception = new ServiceException(ResultCode.GOODS_NOT_EXIST);

        Assertions.assertEquals(ResultCode.GOODS_NOT_EXIST, exception.getResultCode());
        Assertions.assertEquals(ResultCode.GOODS_NOT_EXIST.message(), exception.getMsg());
        Assertions.assertEquals(ResultCode.GOODS_NOT_EXIST.message(), exception.getMessage());
        Assertions.assertNotEquals(ResultCode.ERROR.message(), exception.getMsg());
    }
}
