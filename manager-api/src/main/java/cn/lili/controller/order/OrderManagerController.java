package cn.lili.controller.order;

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
import io.swagger.annotations.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import springfox.documentation.annotations.ApiIgnore;

import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.util.List;

/**
 * 管理端,订单API
 *
 * @author Chopper
 * @since 2020/11/17 4:34 下午
 */
@RestController
@RequestMapping("/manager/order/order")
@Api(tags = "管理端,订单API")
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


    @ApiOperation(value = "查询订单列表分页")
    @GetMapping
    public ResultMessage<IPage<OrderSimpleVO>> queryMineOrder(OrderSearchParams orderSearchParams) {
        return ResultUtil.data(orderService.queryByParams(orderSearchParams));
    }

    @ApiOperation(value = "获取订单数量")
    @GetMapping(value = "/orderNum")
    public ResultMessage<OrderNumVO> getOrderNumVO(OrderSearchParams orderSearchParams) {
        return ResultUtil.data(orderService.getOrderNumVO(orderSearchParams));
    }

    @ApiOperation(value = "查询订单导出列表")
    @GetMapping("/queryExportOrder")
    public void queryExportOrder(OrderSearchParams orderSearchParams) {
        HttpServletResponse response = ThreadContextHolder.getHttpResponse();
        orderService.queryExportOrder(response,orderSearchParams);
    }


    @ApiOperation(value = "订单明细")
    @ApiImplicitParam(name = "orderSn", value = "订单编号", required = true, dataType = "String", paramType = "path")
    @GetMapping(value = "/{orderSn}")
    public ResultMessage<OrderDetailVO> detail(@PathVariable String orderSn) {
        return ResultUtil.data(orderService.queryDetail(orderSn));
    }


    @PreventDuplicateSubmissions
    @ApiOperation(value = "确认收款")
    @ApiImplicitParam(name = "orderSn", value = "订单编号", required = true, dataType = "String", paramType = "path")
    @PostMapping(value = "/{orderSn}/pay")
    public ResultMessage<Object> payOrder(@PathVariable String orderSn) {
        orderPriceService.adminPayOrder(orderSn);
        return ResultUtil.success();
    }

    @PreventDuplicateSubmissions
    @ApiOperation(value = "修改收货人信息")
    @ApiImplicitParam(name = "orderSn", value = "订单sn", required = true, dataType = "String", paramType = "path")
    @PostMapping(value = "/update/{orderSn}/consignee")
    public ResultMessage<Order> consignee(@NotNull(message = "参数非法") @PathVariable String orderSn,
                                          @Valid MemberAddressDTO memberAddressDTO) {
        return ResultUtil.data(orderService.updateConsignee(orderSn, memberAddressDTO));
    }

    @PreventDuplicateSubmissions
    @ApiOperation(value = "修改订单价格")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "orderSn", value = "订单sn", required = true, dataType = "String", paramType = "path"),
            @ApiImplicitParam(name = "price", value = "订单价格", required = true, dataType = "Double", paramType = "query"),
    })
    @PutMapping(value = "/update/{orderSn}/price")
    public ResultMessage<Order> updateOrderPrice(@PathVariable String orderSn,
                                                 @NotNull(message = "订单价格不能为空") @RequestParam Double price) {
        if (NumberUtil.isGreater(Convert.toBigDecimal(price), Convert.toBigDecimal(0))) {
            return ResultUtil.data(orderPriceService.updatePrice(orderSn, price));
        } else {
            return ResultUtil.error(ResultCode.ORDER_PRICE_ERROR);
        }
    }


    @PreventDuplicateSubmissions
    @ApiOperation(value = "取消订单")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "orderSn", value = "订单编号", required = true, dataType = "String", paramType = "path"),
            @ApiImplicitParam(name = "reason", value = "取消原因", required = true, dataType = "String", paramType = "query")
    })
    @PostMapping(value = "/{orderSn}/cancel")
    public ResultMessage<Order> cancel(@ApiIgnore @PathVariable String orderSn, @RequestParam String reason) {
        return ResultUtil.data(orderService.cancel(orderSn, reason));
    }


    @ApiOperation(value = "查询物流踪迹")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "orderSn", value = "订单编号", required = true, dataType = "String", paramType = "path")
    })
    @PostMapping(value = "/getTraces/{orderSn}")
    public ResultMessage<Object> getTraces(@NotBlank(message = "订单编号不能为空") @PathVariable String orderSn) {
        return ResultUtil.data(orderService.getTraces(orderSn));
    }

    @ApiOperation(value = "卖家订单备注")
    @PutMapping("/{orderSn}/sellerRemark")
    public ResultMessage<Object> sellerRemark(@PathVariable String orderSn, @RequestParam String sellerRemark) {
        orderService.updateSellerRemark(orderSn, sellerRemark);
        return ResultUtil.success();
    }
}