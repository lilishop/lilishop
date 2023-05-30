package cn.lili.controller.payment;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.payment.kit.CashierSupport;
import cn.lili.modules.payment.kit.dto.PayParam;
import cn.lili.modules.payment.entity.enums.PaymentClientEnum;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.modules.payment.kit.params.dto.CashierParam;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * 买家端,收银台接口
 *
 * @author Chopper
 * @since 2020-12-18 16:59
 */
@Slf4j
@RestController
@Api(tags = "买家端,收银台接口")
@RequestMapping("/buyer/payment/cashier")
public class CashierController {

    @Autowired
    private CashierSupport cashierSupport;


    @ApiImplicitParams({
            @ApiImplicitParam(name = "client", value = "客户端类型", paramType = "path", allowableValues = "PC,H5,WECHAT_MP,APP")
    })
    @GetMapping(value = "/tradeDetail")
    @ApiOperation(value = "获取支付详情")
    public ResultMessage paymentParams(@Validated PayParam payParam) {
        CashierParam cashierParam = cashierSupport.cashierParam(payParam);
        return ResultUtil.data(cashierParam);
    }


    @ApiImplicitParams({
            @ApiImplicitParam(name = "paymentMethod", value = "支付方式", paramType = "path", allowableValues = "WECHAT,ALIPAY"),
            @ApiImplicitParam(name = "paymentClient", value = "调起方式", paramType = "path", allowableValues = "APP,NATIVE,JSAPI,H5,MP")
    })
    @GetMapping(value = "/pay/{paymentMethod}/{paymentClient}")
    @ApiOperation(value = "支付")
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

    @ApiOperation(value = "支付回调")
    @RequestMapping(value = "/callback/{paymentMethod}", method = {RequestMethod.GET, RequestMethod.POST})
    public ResultMessage<Object> callback(HttpServletRequest request, @PathVariable String paymentMethod) {

        PaymentMethodEnum paymentMethodEnum = PaymentMethodEnum.valueOf(paymentMethod);

        cashierSupport.callback(paymentMethodEnum, request);

        return ResultUtil.success(ResultCode.PAY_SUCCESS);
    }

    @ApiOperation(value = "支付异步通知")
    @RequestMapping(value = "/notify/{paymentMethod}", method = {RequestMethod.GET, RequestMethod.POST})
    public void notify(HttpServletRequest request, @PathVariable String paymentMethod) {

        PaymentMethodEnum paymentMethodEnum = PaymentMethodEnum.valueOf(paymentMethod);

        cashierSupport.notify(paymentMethodEnum, request);

    }

    @ApiOperation(value = "查询支付结果")
    @GetMapping(value = "/result")
    public ResultMessage<Boolean> paymentResult(PayParam payParam) {
        return ResultUtil.data(cashierSupport.paymentResult(payParam));
    }
}
