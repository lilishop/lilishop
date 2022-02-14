package cn.lili.controller.order;

import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.context.ThreadContextHolder;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.dto.MemberAddressDTO;
import cn.lili.modules.member.service.StoreLogisticsService;
import cn.lili.modules.order.order.entity.dto.OrderExportDTO;
import cn.lili.modules.order.order.entity.dto.OrderSearchParams;
import cn.lili.modules.order.order.entity.vo.OrderDetailVO;
import cn.lili.modules.order.order.entity.vo.OrderSimpleVO;
import cn.lili.modules.order.order.service.OrderPriceService;
import cn.lili.modules.order.order.service.OrderService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.util.List;
import java.util.Objects;

/**
 * 店铺端,订单接口
 *
 * @author Chopper
 * @since 2020/11/17 4:35 下午
 **/
@Slf4j
@RestController
@RequestMapping("/store/order/order")
@Api(tags = "店铺端,订单接口")
public class OrderStoreController {

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
    /**
     * 物流公司
     */
    @Autowired
    private StoreLogisticsService storeLogisticsService;


    @ApiOperation(value = "查询订单列表")
    @GetMapping
    public ResultMessage<IPage<OrderSimpleVO>> queryMineOrder(OrderSearchParams orderSearchParams) {
        return ResultUtil.data(orderService.queryByParams(orderSearchParams));
    }


    @ApiOperation(value = "订单明细")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "orderSn", value = "订单编号", required = true, dataType = "String", paramType = "path")
    })
    @GetMapping(value = "/{orderSn}")
    public ResultMessage<OrderDetailVO> detail(@NotNull @PathVariable String orderSn) {
        OperationalJudgment.judgment(orderService.getBySn(orderSn));
        return ResultUtil.data(orderService.queryDetail(orderSn));
    }

    @ApiOperation(value = "修改收货人信息")
    @ApiImplicitParam(name = "orderSn", value = "订单sn", required = true, dataType = "String", paramType = "path")
    @PostMapping(value = "/update/{orderSn}/consignee")
    public ResultMessage<Object> consignee(@NotNull(message = "参数非法") @PathVariable String orderSn,
                                           @Valid MemberAddressDTO memberAddressDTO) {
        return ResultUtil.data(orderService.updateConsignee(orderSn, memberAddressDTO));
    }

    @PreventDuplicateSubmissions
    @ApiOperation(value = "修改订单价格")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "orderSn", value = "订单sn", required = true, dataType = "String", paramType = "path"),
            @ApiImplicitParam(name = "orderPrice", value = "订单价格", required = true, dataType = "Double", paramType = "query"),
    })
    @PutMapping(value = "/update/{orderSn}/price")
    public ResultMessage<Object> updateOrderPrice(@PathVariable String orderSn,
                                                  @NotNull(message = "订单价格不能为空") @RequestParam Double orderPrice) {
        return ResultUtil.data(orderPriceService.updatePrice(orderSn, orderPrice));
    }

    @PreventDuplicateSubmissions
    @ApiOperation(value = "订单发货")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "orderSn", value = "订单sn", required = true, dataType = "String", paramType = "path"),
            @ApiImplicitParam(name = "logisticsNo", value = "发货单号", required = true, dataType = "String", paramType = "query"),
            @ApiImplicitParam(name = "logisticsId", value = "物流公司", required = true, dataType = "String", paramType = "query")
    })
    @PostMapping(value = "/{orderSn}/delivery")
    public ResultMessage<Object> delivery(@NotNull(message = "参数非法") @PathVariable String orderSn,
                                          @NotNull(message = "发货单号不能为空") String logisticsNo,
                                          @NotNull(message = "请选择物流公司") String logisticsId) {
        return ResultUtil.data(orderService.delivery(orderSn, logisticsNo, logisticsId));
    }

    @PreventDuplicateSubmissions
    @ApiOperation(value = "取消订单")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "orderSn", value = "订单编号", required = true, dataType = "String", paramType = "path"),
            @ApiImplicitParam(name = "reason", value = "取消原因", required = true, dataType = "String", paramType = "query")
    })
    @PostMapping(value = "/{orderSn}/cancel")
    public ResultMessage<Object> cancel(@PathVariable String orderSn, @RequestParam String reason) {
        return ResultUtil.data(orderService.cancel(orderSn, reason));
    }

    @ApiOperation(value = "根据核验码获取订单信息")
    @ApiImplicitParam(name = "verificationCode", value = "核验码", required = true, paramType = "path")
    @GetMapping(value = "/getOrderByVerificationCode/{verificationCode}")
    public ResultMessage<Object> getOrderByVerificationCode(@PathVariable String verificationCode) {
        return ResultUtil.data(orderService.getOrderByVerificationCode(verificationCode));
    }

    @PreventDuplicateSubmissions
    @ApiOperation(value = "订单核验")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "orderSn", value = "订单号", required = true, paramType = "path"),
            @ApiImplicitParam(name = "verificationCode", value = "核验码", required = true, paramType = "path")
    })
    @PutMapping(value = "/take/{orderSn}/{verificationCode}")
    public ResultMessage<Object> take(@PathVariable String orderSn, @PathVariable String verificationCode) {
        return ResultUtil.data(orderService.take(orderSn, verificationCode));
    }

    @ApiOperation(value = "查询物流踪迹")
    @ApiImplicitParam(name = "orderSn", value = "订单编号", required = true, dataType = "String", paramType = "path")
    @GetMapping(value = "/getTraces/{orderSn}")
    public ResultMessage<Object> getTraces(@NotBlank(message = "订单编号不能为空") @PathVariable String orderSn) {
        OperationalJudgment.judgment(orderService.getBySn(orderSn));
        return ResultUtil.data(orderService.getTraces(orderSn));
    }

    @ApiOperation(value = "下载待发货的订单列表", produces = "application/octet-stream")
    @GetMapping(value = "/downLoadDeliverExcel")
    public void downLoadDeliverExcel() {
        HttpServletResponse response = ThreadContextHolder.getHttpResponse();
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        //获取店铺已经选择物流公司列表
        List<String> logisticsName = storeLogisticsService.getStoreSelectedLogisticsName(storeId);
        //下载订单批量发货Excel
        this.orderService.getBatchDeliverList(response, logisticsName);

    }

    @PostMapping(value = "/batchDeliver", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    @ApiOperation(value = "上传文件进行订单批量发货")
    public ResultMessage<Object> batchDeliver(@RequestPart("files") MultipartFile files) {
        orderService.batchDeliver(files);
        return ResultUtil.success(ResultCode.SUCCESS);
    }

    @ApiOperation(value = "查询订单导出列表")
    @GetMapping("/queryExportOrder")
    public ResultMessage<List<OrderExportDTO>> queryExportOrder(OrderSearchParams orderSearchParams) {
        return ResultUtil.data(orderService.queryExportOrder(orderSearchParams));
    }
}