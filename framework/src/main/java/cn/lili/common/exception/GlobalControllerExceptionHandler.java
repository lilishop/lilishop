package cn.lili.common.exception;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.validation.BindException;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.ConstraintViolationException;
import java.util.List;
import java.util.stream.Collectors;

/**
 * 异常处理
 *
 * @author Chopper
 */
@RestControllerAdvice
@Slf4j
public class GlobalControllerExceptionHandler {

    /**
     * 如果超过长度，则前后段交互体验不佳，使用默认错误消息
     */
    static Integer MAX_LENGTH = 200;

    /**
     * 自定义异常
     *
     * @param e
     * @return
     */
    @ExceptionHandler(ServiceException.class)
    @ResponseStatus(value = HttpStatus.BAD_REQUEST)
    public ResultMessage<Object> handleServiceException(HttpServletRequest request, final Exception e, HttpServletResponse response) {

        //如果是自定义异常，则获取异常，返回自定义错误消息
        if (e instanceof ServiceException) {

            ServiceException serviceException = ((ServiceException) e);
            ResultCode resultCode = serviceException.getResultCode();

            Integer code = resultCode.code();
            String message = resultCode.message();

            //如果有扩展消息，则输出异常中，跟随补充异常
            if (message != null) {
                message = appendErrorMessage(message, serviceException.getMsg());
            }

            // 对一些特殊异常处理，不再打印error级别的日志
            if (serviceException.getResultCode().equals(ResultCode.DEMO_SITE_EXCEPTION)) {
                log.debug("[DEMO_SITE_EXCEPTION]:{}", serviceException.getResultCode().message(), e);
                return ResultUtil.error(code, message);
            }
            if (serviceException.getResultCode().equals(ResultCode.USER_AUTH_EXPIRED)) {
                log.debug("403 :{}", serviceException.getResultCode().message(), e);
                return ResultUtil.error(code, message);
            }


            log.error("全局异常[ServiceException]:{}-{}", serviceException.getResultCode().code(), serviceException.getResultCode().message(), e);
            return ResultUtil.error(code, message);

        } else {

            log.error("全局异常[ServiceException]:", e);
        }

        //默认错误消息
        String errorMsg = "服务器异常，请稍后重试";
        if (e != null && e.getMessage() != null && e.getMessage().length() < MAX_LENGTH) {
            errorMsg = e.getMessage();
        }
        return ResultUtil.error(ResultCode.ERROR.code(), errorMsg);
    }
/*
    @ExceptionHandler(ConstraintViolationException.class)
    @ResponseStatus(value = HttpStatus.BAD_REQUEST)
    public ResultMessage<Object> constraintExceptionHandler(HttpServletRequest request, final Exception e, HttpServletResponse response) {

        log.error("全局异常[RuntimeException]:", e);

        return ResultUtil.error(001, e.getMessage());
    }*/

    @ExceptionHandler(RuntimeException.class)
    @ResponseStatus(value = HttpStatus.BAD_REQUEST)
    public ResultMessage<Object> runtimeExceptionHandler(HttpServletRequest request, final Exception e, HttpServletResponse response) {

        log.error("全局异常[RuntimeException]:", e);

        // 检查异常链是否包含 ServiceException
        ServiceException serviceException = findServiceException(e);

        if (serviceException != null) {
            ResultCode resultCode = serviceException.getResultCode();
            Integer code = resultCode.code();
            String message = resultCode.message();
            if (message != null) {
                message = appendErrorMessage(message, serviceException.getMsg());
            }
            return ResultUtil.error(code, message);
        }
        return ResultUtil.error();
    }

    // 遍历异常链，查找 ServiceException
    private ServiceException findServiceException(Throwable ex) {
        while (ex != null) {
            if (ex instanceof ServiceException) {
                return (ServiceException) ex;
            }
            ex = ex.getCause();
        }
        return null;
    }

//   /**
//    * 通用的接口映射异常处理方
//    */
//   @Override
//   protected ResponseEntity<Object> handleExceptionInternal(Exception ex, Object body, HttpHeaders headers, HttpStatus status, WebRequest request) {
//       if (ex instanceof MethodArgumentNotValidException) {
//           MethodArgumentNotValidException exception = (MethodArgumentNotValidException) ex;
//           return new ResponseEntity<>(new ResultUtil<>().setErrorMsg(exception.getBindingResult().getAllErrors().get(0).getDefaultMessage()),
//           status);
//       }
//       if (ex instanceof MethodArgumentTypeMismatchException) {
//           MethodArgumentTypeMismatchException exception = (MethodArgumentTypeMismatchException) ex;
//           logger.error("参数转换失败，方法：" + exception.getParameter().getMethod().getName() + "，参数：" + exception.getName()
//                   + ",信息：" + exception.getLocalizedMessage());
//           return new ResponseEntity<>(new ResultUtil<>().setErrorMsg("参数转换失败"), status);
//       }
//       ex.printStackTrace();
//       return new ResponseEntity<>(new ResultUtil<>().setErrorMsg("未知异常，请联系管理员"), status);
//   }

    /**
     * bean校验未通过异常
     *
     * @see javax.validation.Valid
     * @see org.springframework.validation.Validator
     * @see org.springframework.validation.DataBinder
     */
    @ExceptionHandler(BindException.class)
    @ResponseStatus(HttpStatus.BAD_REQUEST)
    @ResponseBody
    public ResultMessage<Object> validExceptionHandler(HttpServletRequest request, final Exception e, HttpServletResponse response) {

        BindException exception = (BindException) e;
        List<FieldError> fieldErrors = exception.getBindingResult().getFieldErrors();
        // 错误消息处理
        try {
            if (!fieldErrors.isEmpty()) {
                return ResultUtil.error(ResultCode.PARAMS_ERROR.code(),
                        fieldErrors.stream()
                                // 获取每个对象的名称字段
                                .map(FieldError::getDefaultMessage)
                                .collect(Collectors.joining(", ")));
            }
            return ResultUtil.error(ResultCode.PARAMS_ERROR);
        } catch (Exception ex) {
            return ResultUtil.error(ResultCode.PARAMS_ERROR);
        }
    }

    /**
     * bean校验未通过异常
     *
     * @see javax.validation.Valid
     * @see org.springframework.validation.Validator
     * @see org.springframework.validation.DataBinder
     */
    @ExceptionHandler(ConstraintViolationException.class)
    @ResponseStatus(HttpStatus.BAD_REQUEST)
    @ResponseBody
    public ResultMessage<Object> constraintViolationExceptionHandler(HttpServletRequest request, final Exception e, HttpServletResponse response) {
        ConstraintViolationException exception = (ConstraintViolationException) e;
        return ResultUtil.error(ResultCode.PARAMS_ERROR.code(), exception.getMessage());
    }

    /**
     * 拼接错误消息
     *
     * @param message       原始消息
     * @param appendMessage 需要拼接的消息
     * @return 拼接后的消息
     */
    private String appendErrorMessage(String message, String appendMessage) {

        //这里的代码看起来有点乱，简单解释一下
        //场景1：服务A，服务B=》
        // 服务A调用服务B=》
        // 服务B抛出异常{扩展消息}，拼接后成为{默认消息}：{扩展消息}
        // 异常被服务A捕获=》
        // 最终消息拼接过程中，当前方法体参数message是{默认消息}，参数appendMessage是服务A给的{默认消息}+{扩展消息}，最终会形成{默认消息}+{默认消息}+{扩展消息}
        //场景2：只有服务A=》
        // 服务A抛出异常{扩展消息}=》
        // 当前方法体拼接{默认消息}：{扩展消息} 并输出返回。
        //
        //总的来说，由于消息拼接是一个流式传递，由服务间传递，所以这里的消息可能存在A包含B，也可能出现B包含A，
        // 所以这里需要双重判定，A包含B=》返回A，B包含A=》返回B，否则返回拼接后的消息

        if (message.contains(appendMessage)) {
            return message;
        }
        if (appendMessage.contains(message)) {
            return appendMessage;
        }
        //忽略默认错误信息，如果有其他错误消息体就不再返回默认的错误消息
        if (message.equals(ResultCode.ERROR.message())) {
            return appendMessage;
        }
        if (appendMessage.equals(ResultCode.ERROR.message())) {
            return message;
        }
        return CharSequenceUtil.format("{}-{}", message, appendMessage);
    }
}
