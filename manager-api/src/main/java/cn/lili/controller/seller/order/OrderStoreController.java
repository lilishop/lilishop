package cn.lili.controller.seller.order;

import cn.hutool.core.convert.Convert;
import cn.hutool.core.util.NumberUtil;
import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.context.ThreadContextHolder;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.dto.MemberAddressDTO;
import cn.lili.modules.member.service.StoreLogisticsService;
import cn.lili.modules.order.order.entity.dto.OrderSearchParams;
import cn.lili.modules.order.order.entity.dto.PartDeliveryParamsDTO;
import cn.lili.modules.order.order.entity.vo.OrderDetailVO;
import cn.lili.modules.order.order.entity.vo.OrderNumVO;
import cn.lili.modules.order.order.entity.vo.OrderSimpleVO;
import cn.lili.modules.order.order.service.OrderPackageService;
import cn.lili.modules.order.order.service.OrderPriceService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.system.service.LogisticsService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
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
@Tag(name = "店铺端,订单接口")
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

    /**
     * 快递
     */
    @Autowired
    private LogisticsService logisticsService;

    @Autowired
    private OrderPackageService orderPackageService;


    @Operation(description = "查询订单列表")
    @GetMapping
    public ResultMessage<IPage<OrderSimpleVO>> queryMineOrder(OrderSearchParams orderSearchParams) {
        return ResultUtil.data(orderService.queryByParams(orderSearchParams));
    }

    @Operation(description = "获取订单数量")
    @GetMapping("/orderNum")
    public ResultMessage<OrderNumVO> getOrderNumVO(OrderSearchParams orderSearchParams) {
        return ResultUtil.data(orderService.getOrderNumVO(orderSearchParams));
    }

    @Operation(description = "订单明细")
    @Parameter(name = "orderSn", description = "订单编号", required = true)
    @GetMapping("/{orderSn}")
    public ResultMessage<OrderDetailVO> detail(@NotNull @PathVariable String orderSn) {
        OperationalJudgment.judgment(orderService.getBySn(orderSn));
        return ResultUtil.data(orderService.queryDetail(orderSn));
    }

    @PreventDuplicateSubmissions
    @Operation(description = "修改收货人信息")
    @Parameter(name = "orderSn", description = "订单sn", required = true)
    @PostMapping("/update/{orderSn}/consignee")
    public ResultMessage<Object> consignee(@NotNull(message = "参数非法") @PathVariable String orderSn,
                                           @Valid MemberAddressDTO memberAddressDTO) {
        return ResultUtil.data(orderService.updateConsignee(orderSn, memberAddressDTO));
    }

    @PreventDuplicateSubmissions
    @Operation(description = "修改订单价格")
    @Parameter(name = "orderSn", description = "订单sn", required = true)
    @Parameter(name = "orderPrice", description = "订单价格", required = true)
    @PutMapping("/update/{orderSn}/price")
    public ResultMessage<Object> updateOrderPrice(@PathVariable String orderSn,
                                                  @NotNull(message = "订单价格不能为空") @RequestParam Double orderPrice) {
        if (NumberUtil.isGreater(Convert.toBigDecimal(orderPrice), Convert.toBigDecimal(0))) {
            return ResultUtil.data(orderPriceService.updatePrice(orderSn, orderPrice));
        } else {
            return ResultUtil.error(ResultCode.ORDER_PRICE_ERROR);
        }
    }

    @PreventDuplicateSubmissions
    @Operation(description = "订单发货")
    @Parameter(name = "orderSn", description = "订单sn", required = true)
    @Parameter(name = "logisticsNo", description = "发货单号", required = true)
    @Parameter(name = "logisticsId", description = "物流公司", required = true)
    @PostMapping("/{orderSn}/delivery")
    public ResultMessage<Object> delivery(@NotNull(message = "参数非法") @PathVariable String orderSn,
                                          @NotNull(message = "发货单号不能为空") String logisticsNo,
                                          @NotNull(message = "请选择物流公司") String logisticsId) {
        return ResultUtil.data(orderService.delivery(orderSn, logisticsNo, logisticsId));
    }

    @PreventDuplicateSubmissions
    @Operation(description = "订单顺丰发货")
    @Parameter(name = "orderSn", description = "订单sn", required = true)
    @PostMapping("/{orderSn}/shunfeng/delivery")
    public ResultMessage<Object> shunFengDelivery(@NotNull(message = "参数非法") @PathVariable String orderSn) {
        return ResultUtil.data(orderService.shunFengDelivery(orderSn));
    }

    @PreventDuplicateSubmissions
    @Operation(description = "取消订单")
    @Parameter(name = "orderSn", description = "订单sn", required = true)
    @Parameter(name = "reason", description = "取消原因", required = true)
    @PostMapping("/{orderSn}/cancel")
    public ResultMessage<Object> cancel(@PathVariable String orderSn, @RequestParam String reason) {
        return ResultUtil.data(orderService.cancel(orderSn, reason));
    }


    @Operation(description = "根据核验码获取订单信息")
    @Parameter(name = "verificationCode", description = "核验码", required = true)
    @GetMapping("/getOrderByVerificationCode/{verificationCode}")
    public ResultMessage<Object> getOrderByVerificationCode(@PathVariable String verificationCode) {
        return ResultUtil.data(orderService.getOrderByVerificationCode(verificationCode));
    }

    @PreventDuplicateSubmissions
    @Operation(description = "订单核验")
    @Parameter(name = "orderSn", description = "订单sn", required = true)
    @Parameter(name = "verificationCode", description = "核验码", required = true)
    @PutMapping("/take/{orderSn}/{verificationCode}")
    public ResultMessage<Object> take(@PathVariable String orderSn, @PathVariable String verificationCode) {
        return ResultUtil.data(orderService.take(orderSn, verificationCode));
    }

    @Operation(description = "查询物流踪迹")
    @Parameter(name = "orderSn", description = "订单sn", required = true)
    @GetMapping("/getTraces/{orderSn}")
    public ResultMessage<Object> getTraces(@NotBlank(message = "订单编号不能为空") @PathVariable String orderSn) {
        OperationalJudgment.judgment(orderService.getBySn(orderSn));
        return ResultUtil.data(orderService.getTraces(orderSn));
    }

    @Operation(description = "下载待发货的订单列表")
    @GetMapping(value = "/downLoadDeliverExcel", produces = "application/octet-stream")
    public void downLoadDeliverExcel() {
        HttpServletResponse response = ThreadContextHolder.getHttpResponse();
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        //获取店铺已经选择物流公司列表
        List<String> logisticsName = storeLogisticsService.getStoreSelectedLogisticsName(storeId);
        //下载订单批量发货Excel
        this.orderService.getBatchDeliverList(response, logisticsName);

    }


    @Operation(description = "上传文件进行订单批量发货")
    @PostMapping(value="/batchDeliver", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    @Parameter(name = "files", description = "订单批量发货文件", required = true)
    public ResultMessage<Object> batchDeliver(@RequestPart("files") MultipartFile files) {
        orderService.batchDeliver(files);
        return ResultUtil.success(ResultCode.SUCCESS);
    }

    @Operation(description = "查询订单导出列表")
    @Parameter(name = "orderSearchParams", description = "订单查询参数", required = true)
    @GetMapping("/queryExportOrder")
    public void queryExportOrder(OrderSearchParams orderSearchParams) {
        HttpServletResponse response = ThreadContextHolder.getHttpResponse();
        orderService.queryExportOrder(response,orderSearchParams);
    }

    @PreventDuplicateSubmissions
    @Operation(description = "创建电子面单")
    @Parameter(name = "orderSn", description = "订单sn", required = true)
    @Parameter(name = "logisticsId", description = "物流公司", required = true)
    @PostMapping("/{orderSn}/createElectronicsFaceSheet")
    public ResultMessage<Object> createElectronicsFaceSheet(@NotNull(message = "参数非法") @PathVariable String orderSn,
                                                            @NotNull(message = "请选择物流公司") String logisticsId) {
        return ResultUtil.data(logisticsService.labelOrder(orderSn, logisticsId));
    }

    
    @Operation(description = "查看包裹列表")
    @Parameter(name = "orderSn", description = "订单sn", required = true)
    @GetMapping("/getPackage/{orderSn}")
    public ResultMessage<Object> getPackage(@NotBlank(message = "订单编号不能为空") @PathVariable String orderSn) {
        return ResultUtil.data(orderPackageService.getOrderPackageVOList(orderSn));
    }

    @Operation(description = "查询物流踪迹")
    @Parameter(name = "orderSn", description = "订单sn", required = true)
    @GetMapping("/getTracesList/{orderSn}")
    public ResultMessage<Object> getTracesList(@NotBlank(message = "订单编号不能为空") @PathVariable String orderSn) {
        return ResultUtil.data(orderPackageService.getOrderPackageVOList(orderSn));
    }

    @Operation(description = "订单包裹发货")
    @Parameter(name = "orderSn", description = "订单sn", required = true)
    @Parameter(name = "logisticsNo", description = "发货单号", required = true)
    @Parameter(name = "logisticsId", description = "物流公司", required = true)
    @PostMapping("/{orderSn}/partDelivery")
    public ResultMessage<Object> delivery(@RequestBody PartDeliveryParamsDTO partDeliveryParamsDTO) {
        return ResultUtil.data(orderService.partDelivery(partDeliveryParamsDTO));
    }

    @Operation(description = "卖家订单备注")
    @Parameter(name = "orderSn", description = "订单sn", required = true)
    @Parameter(name = "sellerRemark", description = "卖家备注", required = true)
    @PutMapping("/{orderSn}/sellerRemark")
    public ResultMessage<Object> sellerRemark(@PathVariable String orderSn, @RequestParam String sellerRemark) {
        orderService.updateSellerRemark(orderSn, sellerRemark);
        return ResultUtil.success();
    }
}