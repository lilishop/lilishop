package cn.lili.controller.payment;

import cn.lili.modules.payment.kit.RefundSupport;
import cn.lili.modules.payment.kit.enums.PaymentMethodEnum;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import javax.servlet.http.HttpServletRequest;

/**
 * 买家端,退款回调
 *
 * @author Chopper
 * @date 2020-12-18 16:59
 */
@Api(tags = "买家端,退款回调")
@RestController
@RequestMapping("/buyer/cashier/refund")
public class CashierRefundController {

    @Autowired
    private RefundSupport refundSupport;


    @ApiOperation(value = "退款通知")
    @RequestMapping(value = "/notify/{paymentMethod}", method = {RequestMethod.GET, RequestMethod.POST})
    public void notify(HttpServletRequest request, @PathVariable String paymentMethod) {
        PaymentMethodEnum paymentMethodEnum = PaymentMethodEnum.valueOf(paymentMethod);
        refundSupport.notify(paymentMethodEnum, request);
    }
}
