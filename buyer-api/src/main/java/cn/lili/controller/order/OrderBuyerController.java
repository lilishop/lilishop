package cn.lili.controller.order;

import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.service.CardKeyService;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderPackage;
import cn.lili.modules.order.order.entity.dto.OrderSearchParams;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.vo.OrderDetailVO;
import cn.lili.modules.order.order.entity.vo.OrderSimpleVO;
import cn.lili.modules.order.order.service.OrderPackageService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.system.entity.vo.Traces;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import java.util.Objects;

/**
 * 买家端,订单接口
 *
 * @author Chopper
 * @since 2020/11/16 10:08 下午
 */
@RestController
@Tag(name = "买家端,订单接口")
@RequestMapping("/buyer/order/order")
public class OrderBuyerController {

    /**
     * 订单
     */
    @Autowired
    private OrderService orderService;

    @Autowired
    private OrderPackageService orderPackageService;

    @Autowired
    private CardKeyService cardKeyService;

    @Operation(summary = "查询客户订单列表")
    @GetMapping
    public ResultMessage<IPage<OrderSimpleVO>> queryMineOrder(OrderSearchParams orderSearchParams) {
        AuthUser currentUser = Objects.requireNonNull(UserContext.getCurrentUser());
        orderSearchParams.setMemberId(currentUser.getId());
        return ResultUtil.data(orderService.queryBuyerByParams(orderSearchParams));
    }

    @Operation(summary = "订单明细")
    @Parameters({
            @Parameter(name = "orderSn", description = "订单编号", required = true)
    })
    @GetMapping("/{orderSn}")
    public ResultMessage<OrderDetailVO> detail(@NotNull(message = "订单编号不能为空") @PathVariable("orderSn") String orderSn) {
        OrderDetailVO orderDetailVO = orderService.queryDetail(orderSn);
        OperationalJudgment.judgment(orderDetailVO.getOrder());
        cardKeyService.enrichOrderDetail(orderDetailVO);
        return ResultUtil.data(orderDetailVO);
    }

    @PreventDuplicateSubmissions
    @Operation(summary = "确认收货")
    @Parameters({
            @Parameter(name = "orderSn", description = "订单编号", required = true)
    })
    @PostMapping("/{orderSn}/receiving")
    public ResultMessage<Object> receiving(@NotNull(message = "订单编号不能为空") @PathVariable("orderSn") String orderSn) {
        Order order = orderService.getBySn(orderSn);
        if (order == null) {
            throw new ServiceException(ResultCode.ORDER_NOT_EXIST);
        }
        //判定是否是待收货状态
        if (!order.getOrderStatus().equals(OrderStatusEnum.DELIVERED.name())) {
            throw new ServiceException(ResultCode.ORDER_DELIVERED_ERROR);
        }
        orderService.complete(orderSn);
        return ResultUtil.success();
    }

    @PreventDuplicateSubmissions
    @Operation(summary = "取消订单")
    @Parameters({
            @Parameter(name = "orderSn", description = "订单编号", required = true),
            @Parameter(name = "reason", description = "取消原因", required = true)
    })
    @PostMapping("/{orderSn}/cancel")
    public ResultMessage<Object> cancel(@PathVariable String orderSn, @RequestParam String reason) {
        orderService.cancel(orderSn, reason);
        return ResultUtil.success();
    }

    @PreventDuplicateSubmissions
    @Operation(summary = "删除订单")
    @Parameters({
            @Parameter(name = "orderSn", description = "订单编号", required = true)
    })
    @DeleteMapping("/{orderSn}")
    public ResultMessage<Object> deleteOrder(@PathVariable String orderSn) {
        OperationalJudgment.judgment(orderService.getBySn(orderSn));
        orderService.deleteOrder(orderSn);
        return ResultUtil.success();
    }

    @Operation(summary = "查询物流踪迹")
    @Parameters({
            @Parameter(name = "orderSn", description = "订单编号", required = true)
    })
    @PostMapping("/getTraces/{orderSn}")
    public ResultMessage<Object> getTraces(@NotBlank(message = "订单编号不能为空") @PathVariable String orderSn) {
        OperationalJudgment.judgment(orderService.getBySn(orderSn));
        return ResultUtil.data(orderService.getTraces(orderSn));
    }

    @Operation(summary = "查询地图版物流踪迹")
    @Parameters({
            @Parameter(name = "orderSn", description = "订单编号", required = true)
    })
    @PostMapping("/getMapTraces/{orderSn}")
    public ResultMessage<Object> getMapTraces(@NotBlank(message = "订单编号不能为空") @PathVariable String orderSn) {
        OperationalJudgment.judgment(orderService.getBySn(orderSn));
        return ResultUtil.data(orderService.getMapTraces(orderSn));
    }


    @PreventDuplicateSubmissions
    @Operation(summary = "开票")
    @Parameters({
            @Parameter(name = "orderSn", description = "订单编号", required = true)
    })
    @PostMapping("/receipt/{orderSn}")
    public ResultMessage<Object> invoice(@NotBlank(message = "订单编号不能为空") @PathVariable String orderSn) {
        OperationalJudgment.judgment(orderService.getBySn(orderSn));
        return ResultUtil.data(orderService.invoice(orderSn));
    }

    @Operation(summary = "查询物流踪迹")
    @Parameters({
            @Parameter(name = "orderSn", description = "订单编号", required = true)
    })
    @GetMapping("/getTracesList/{orderSn}")
    public ResultMessage<Object> getTracesList(@NotBlank(message = "订单编号不能为空") @PathVariable String orderSn) {
        return ResultUtil.data(orderPackageService.getOrderPackageVOList(orderSn));
    }

    @Operation(summary = "查看包裹列表")
    @Parameters({
            @Parameter(name = "orderSn", description = "订单编号", required = true)
    })
    @GetMapping("/getPackage/{orderSn}")
    public ResultMessage<Object> getPackage(@NotBlank(message = "订单编号不能为空") @PathVariable String orderSn) {
        return ResultUtil.data(orderPackageService.getOrderPackageVOList(orderSn));
    }
}
