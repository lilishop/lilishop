package cn.lili.controller.seller.order;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.trade.entity.dos.OrderLog;
import cn.lili.modules.order.trade.service.OrderLogService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * 店铺端,订单日志接口
 *
 * @author Chopper
 * @since 2020/12/5
 **/
@RestController
@Tag(name = "店铺端,订单日志接口")
@RequestMapping("/store/order/orderLog")
public class OrderLogStoreController {

    @Autowired
    private OrderLogService orderLogService;

    @Autowired
    private OrderService orderService;

    @Operation(description = "通过订单编号获取订单日志")
    @Parameter(name = "orderSn", description = "订单编号", required = true)
    @GetMapping("/{orderSn}")
    public ResultMessage<List<OrderLog>> get(@PathVariable String orderSn) {
        OperationalJudgment.judgment(orderService.getBySn(orderSn));
        return ResultUtil.data(orderLogService.getOrderLog(orderSn));
    }
}
