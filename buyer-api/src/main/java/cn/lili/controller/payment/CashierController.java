package cn.lili.controller.payment;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.payment.entity.enums.PaymentClientEnum;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.modules.payment.kit.CashierSupport;
import cn.lili.modules.payment.kit.dto.PayParam;
import cn.lili.modules.payment.kit.params.dto.CashierParam;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * 买家端,收银台接口
 *
 * @author Chopper
 * @since 2020-12-18 16:59
 */
@Slf4j
@RestController
@Tag(name = "买家端,收银台接口")
@RequestMapping("/buyer/payment/cashier")
public class CashierController {

    @Autowired
    private CashierSupport cashierSupport;


    @Parameter(name = "client", description = "客户端类型")
    @GetMapping("/tradeDetail")
    @Operation(summary = "获取支付详情")
    public ResultMessage paymentParams(@Validated PayParam payParam) {
        CashierParam cashierParam = cashierSupport.cashierParam(payParam);
        return ResultUtil.data(cashierParam);
    }


    @Parameters({
            @Parameter(name = "paymentMethod", description = "支付方式"),
            @Parameter(name = "paymentClient", description = "调起方式"),
            @Parameter(name = "payParam", description = "支付参数")
    })
    @GetMapping("/pay/{paymentMethod}/{paymentClient}")
    @Operation(summary = "支付")
    public ResultMessage payment(
            HttpServletRequest request,
            HttpServletResponse response,
            @PathVariable String paymentMethod,
            @PathVariable String paymentClient,
            @Validated PayParam payParam) {
        PaymentMethodEnum paymentMethodEnum = PaymentMethodEnum.valueOf(paymentMethod);
        PaymentClientEnum paymentClientEnum = PaymentClientEnum.valueOf(paymentClient);

        try {
            return cashierSupport.payment(paymentMethodEnum, paymentClientEnum, request, response, payParam);
        } catch (ServiceException se) {
            log.info("支付异常", se);
            throw se;
        } catch (Exception e) {
            log.error("收银台支付错误", e);
        }
        return null;


    }

    @Operation(summary = "支付回调")
    @RequestMapping(value = "/callback/{paymentMethod}", method = {RequestMethod.GET, RequestMethod.POST})
    public ResultMessage<Object> callback(HttpServletRequest request, @PathVariable String paymentMethod) {

        PaymentMethodEnum paymentMethodEnum = PaymentMethodEnum.valueOf(paymentMethod);

        cashierSupport.callback(paymentMethodEnum, request);

        return ResultUtil.success(ResultCode.PAY_SUCCESS);
    }

    @Operation(summary = "支付异步通知")
    @RequestMapping(value = "/notify/{paymentMethod}", method = {RequestMethod.GET, RequestMethod.POST})
    public void notify(HttpServletRequest request, @PathVariable String paymentMethod) {

        PaymentMethodEnum paymentMethodEnum = PaymentMethodEnum.valueOf(paymentMethod);

        cashierSupport.notify(paymentMethodEnum, request);

    }

    @Operation(summary = "查询支付结果")
    @GetMapping("/result")
    public ResultMessage<Boolean> paymentResult(PayParam payParam) {
        return ResultUtil.data(cashierSupport.paymentResult(payParam));
    }


    @Operation(summary = "转账异步通知")
    @RequestMapping(value = "/transfer/notify/{paymentMethod}", method = {RequestMethod.GET, RequestMethod.POST})
    public void transferNotifyUrl(HttpServletRequest request, @PathVariable String paymentMethod) {

        PaymentMethodEnum paymentMethodEnum = PaymentMethodEnum.valueOf(paymentMethod);

        cashierSupport.transferNotify(paymentMethodEnum, request);

    }
}
