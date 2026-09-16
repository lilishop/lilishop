package cn.lili.common.exception;

import cn.lili.common.enums.ResultCode;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class ServiceExceptionTest {

    /**
     * 仅传 ResultCode 时，msg / Throwable message 都用该错误码文案，而不是默认「服务器繁忙」。
     */
    @Test
    void resultCodeConstructorUsesResultCodeMessageInsteadOfDefaultBusyText() {
        // GOODS_NOT_EXIST 应对齐自身 message，不能落到 ERROR 的默认文案
        ServiceException exception = new ServiceException(ResultCode.GOODS_NOT_EXIST);

        Assertions.assertEquals(ResultCode.GOODS_NOT_EXIST, exception.getResultCode());
        Assertions.assertEquals(ResultCode.GOODS_NOT_EXIST.message(), exception.getMsg());
        Assertions.assertEquals(ResultCode.GOODS_NOT_EXIST.message(), exception.getMessage());
        Assertions.assertNotEquals(ResultCode.ERROR.message(), exception.getMsg());
    }

    /**
     * 仅传字符串时，错误码为 ERROR，文案同时写入 msg 与 Throwable，避免日志拿到 null。
     */
    @Test
    void stringConstructorPassesMessageToThrowableSoLogsAreNotNull() {
        // 自定义文案要进入 super(message)，getMessage() 才不是 null
        ServiceException exception = new ServiceException("水印处理失败");

        Assertions.assertEquals(ResultCode.ERROR, exception.getResultCode());
        Assertions.assertEquals("水印处理失败", exception.getMsg());
        Assertions.assertEquals("水印处理失败", exception.getMessage());
    }

    /**
     * ResultCode + 自定义文案时，错误码保留，对外展示用传入的 message。
     */
    @Test
    void resultCodeWithCustomMessagePassesMessageToThrowable() {
        // 错误码仍是 ERROR，覆盖文案「水印处理失败」同时给 msg 和 Throwable
        ServiceException exception = new ServiceException(ResultCode.ERROR, "水印处理失败");

        Assertions.assertEquals(ResultCode.ERROR, exception.getResultCode());
        Assertions.assertEquals("水印处理失败", exception.getMsg());
        Assertions.assertEquals("水印处理失败", exception.getMessage());
    }
}
