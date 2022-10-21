package cn.lili.common.exception;

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

            Integer code = null;
            String message = null;

            if (resultCode != null) {
                code = resultCode.code();
                message = resultCode.message();
            }
            //如果有扩展消息，则输出异常中，跟随补充异常
            if (!serviceException.getMsg().equals(ServiceException.DEFAULT_MESSAGE)) {
                message += ":" + serviceException.getMsg();
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

        return ResultUtil.error(ResultCode.ERROR);
    }

//   /**
//    * 通用的接口映射异常处理方
//    */
//   @Override
//   protected ResponseEntity<Object> handleExceptionInternal(Exception ex, Object body, HttpHeaders headers, HttpStatus status, WebRequest request) {
//       if (ex instanceof MethodArgumentNotValidException) {
//           MethodArgumentNotValidException exception = (MethodArgumentNotValidException) ex;
//           return new ResponseEntity<>(new ResultUtil<>().setErrorMsg(exception.getBindingResult().getAllErrors().get(0).getDefaultMessage()), status);
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
        for (FieldError error : fieldErrors) {
            return ResultUtil.error(ResultCode.PARAMS_ERROR.code(), error.getDefaultMessage());
        }
        return ResultUtil.error(ResultCode.PARAMS_ERROR);
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
}
