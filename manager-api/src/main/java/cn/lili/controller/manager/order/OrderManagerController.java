package cn.lili.controller.manager.order;

import cn.hutool.core.convert.Convert;
import cn.hutool.core.util.NumberUtil;
import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.context.ThreadContextHolder;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.dto.MemberAddressDTO;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dto.OrderSearchParams;
import cn.lili.modules.order.order.entity.vo.OrderDetailVO;
import cn.lili.modules.order.order.entity.vo.OrderNumVO;
import cn.lili.modules.order.order.entity.vo.OrderSimpleVO;
import cn.lili.modules.order.order.service.OrderPriceService;
import cn.lili.modules.order.order.service.OrderService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import java.util.List;

/**
 * 管理端,订单API
 *
 * @author Chopper
 * @since 2020/11/17 4:34 下午
 */
@RestController
@RequestMapping("/manager/order/order")
@Tag(name = "管理端,订单API")
public class OrderManagerController {

    /**
     * 订单
     */
    @Autowired
    private OrderService orderService;
    /**
     * 订单价格
     */
    @Autowired
    private OrderPriceService orderPriceService;


    @Operation(summary = "查询订单列表分页")
    @GetMapping
    public ResultMessage<IPage<OrderSimpleVO>> queryMineOrder(OrderSearchParams orderSearchParams) {
        return ResultUtil.data(orderService.queryByParams(orderSearchParams));
    }

    @Operation(summary = "获取订单数量")
    @Parameter(name = "orderSearchParams", description = "查询参数")
    @GetMapping("/orderNum")
    public ResultMessage<OrderNumVO> getOrderNumVO(OrderSearchParams orderSearchParams) {
        return ResultUtil.data(orderService.getOrderNumVO(orderSearchParams));
    }

    @Operation(summary = "查询订单导出列表")
    @GetMapping("/queryExportOrder")
    public void queryExportOrder(OrderSearchParams orderSearchParams) {
        HttpServletResponse response = ThreadContextHolder.getHttpResponse();
        orderService.queryExportOrder(response,orderSearchParams);
    }


    @Operation(summary = "订单明细")
    @Parameter(name = "orderSn", description = "订单编号", required = true)
    @GetMapping("/{orderSn}")
    public ResultMessage<OrderDetailVO> detail(@PathVariable String orderSn) {
        return ResultUtil.data(orderService.queryDetail(orderSn));
    }


    @PreventDuplicateSubmissions
    @Operation(summary = "确认收款")
    @Parameter(name = "orderSn", description = "订单编号", required = true)
    @PostMapping("/{orderSn}/pay")
    public ResultMessage<Object> payOrder(@PathVariable String orderSn) {
        orderPriceService.adminPayOrder(orderSn);
        return ResultUtil.success();
    }

    @PreventDuplicateSubmissions
    @Operation(summary = "修改收货人信息")
    @Parameter(name = "orderSn", description = "订单sn", required = true)
    @PostMapping("/update/{orderSn}/consignee")
    public ResultMessage<Order> consignee(@NotNull(message = "参数非法") @PathVariable String orderSn,
                                          @Valid MemberAddressDTO memberAddressDTO) {
        return ResultUtil.data(orderService.updateConsignee(orderSn, memberAddressDTO));
    }

    @PreventDuplicateSubmissions
    @Operation(summary = "修改订单价格")
    @Parameters({
            @Parameter(name = "orderSn", description = "订单sn", required = true),
            @Parameter(name = "price", description = "订单价格", required = true)
    })
    @PutMapping("/update/{orderSn}/price")
    public ResultMessage<Order> updateOrderPrice(@PathVariable String orderSn,
                                                 @NotNull(message = "订单价格不能为空") @RequestParam Double price) {
        if (NumberUtil.isGreater(Convert.toBigDecimal(price), Convert.toBigDecimal(0))) {
            return ResultUtil.data(orderPriceService.updatePrice(orderSn, price));
        } else {
            return ResultUtil.error(ResultCode.ORDER_PRICE_ERROR);
        }
    }


    @PreventDuplicateSubmissions
    @Operation(summary = "取消订单")
    @Parameters({
            @Parameter(name = "orderSn", description = "订单编号", required = true),
            @Parameter(name = "reason", description = "取消原因", required = true)
    })
    @PostMapping("/{orderSn}/cancel")
    public ResultMessage<Order> cancel(@PathVariable String orderSn, @RequestParam String reason) {
        return ResultUtil.data(orderService.cancel(orderSn, reason));
    }


    @Operation(summary = "查询物流踪迹")
    @Parameters({
            @Parameter(name = "orderSn", description = "订单编号", required = true)
    })
    @PostMapping("/getTraces/{orderSn}")
    public ResultMessage<Object> getTraces(@NotBlank(message = "订单编号不能为空") @PathVariable String orderSn) {
        return ResultUtil.data(orderService.getTraces(orderSn));
    }

    @Operation(summary = "卖家订单备注")
    @PutMapping("/{orderSn}/sellerRemark")
    public ResultMessage<Object> sellerRemark(@PathVariable String orderSn, @RequestParam String sellerRemark) {
        orderService.updateSellerRemark(orderSn, sellerRemark);
        return ResultUtil.success();
    }
}