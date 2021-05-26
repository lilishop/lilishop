package cn.lili.controller.trade;

import cn.hutool.poi.excel.ExcelReader;
import cn.hutool.poi.excel.ExcelUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.dto.MemberAddressDTO;
import cn.lili.modules.order.order.entity.dto.OrderBatchDeliverDTO;
import cn.lili.modules.order.order.entity.dto.OrderSearchParams;
import cn.lili.modules.order.order.entity.vo.OrderDetailVO;
import cn.lili.modules.order.order.entity.vo.OrderSimpleVO;
import cn.lili.modules.order.order.service.OrderPriceService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.system.service.StoreLogisticsService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

/**
 * 店铺端,订单接口
 *
 * @author Chopper
 * @date 2020/11/17 4:35 下午
 **/
@RestController
@RequestMapping("/store/orders")
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

        return ResultUtil.data(orderService.queryDetail(orderSn));
    }

    @ApiOperation(value = "修改收货人信息")
    @ApiImplicitParam(name = "orderSn", value = "订单sn", required = true, dataType = "String", paramType = "path")
    @PostMapping(value = "/update/{orderSn}/consignee")
    public ResultMessage<Object> consignee(@NotNull(message = "参数非法") @PathVariable String orderSn,
                                           @Valid MemberAddressDTO memberAddressDTO) {
        return ResultUtil.data(orderService.updateConsignee(orderSn, memberAddressDTO));
    }

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

    @ApiOperation(value = "取消订单")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "orderSn", value = "订单编号", required = true, dataType = "String", paramType = "path"),
            @ApiImplicitParam(name = "reason", value = "取消原因", required = true, dataType = "String", paramType = "query")
    })
    @PostMapping(value = "/{orderSn}/cancel")
    public ResultMessage<Object> cancel(@PathVariable String orderSn, @RequestParam String reason) {
        return ResultUtil.data(orderService.cancel(orderSn, reason));
    }

    @ApiOperation(value = "订单核验")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "orderSn", value = "订单sn", required = true, dataType = "String", paramType = "path"),
            @ApiImplicitParam(name = "qrCode", value = "发货单号", required = true, dataType = "String", paramType = "query")
    })
    @PostMapping(value = "/{orderSn}/take")
    public ResultMessage<Object> take(@NotNull(message = "参数非法") @PathVariable String orderSn,
                                      @NotNull(message = "核验码") String qrCode) {
        return ResultUtil.data(orderService.take(orderSn, qrCode));
    }

    @ApiOperation(value = "查询物流踪迹")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "orderSn", value = "订单编号", required = true, dataType = "String", paramType = "path")
    })
    @PostMapping(value = "/getTraces/{orderSn}")
    public ResultMessage<Object> getTraces(@NotBlank(message = "订单编号不能为空") @PathVariable String orderSn) {
        return ResultUtil.data(orderService.getTraces(orderSn));
    }

    @ApiOperation(value = "下载待发货的订单列表")
    @GetMapping(value = "/downLoadDeliverExcel")
    public ResultMessage<Object> downLoadDeliverExcel(HttpServletResponse response, List<String> orderIds) {

        //获取店铺已经选择物流公司列表
        List<String> logisticsName = storeLogisticsService.getStoreSelectedLogisticsName();
        //下载订单批量发货Excel
        this.orderService.getBatchDeliverList(response,orderIds,logisticsName);

        return ResultUtil.success(ResultCode.SUCCESS);

    }

    @ApiOperation(value = "上传文件进行订单批量发货")
    @ApiImplicitParam(name = "file", value = "订单列表", required = true, dataType = "file", paramType = "query")
    @PutMapping(value = "/batchDeliver")
    public void batchDeliver(@RequestParam MultipartFile file) {
        InputStream inputStream = null;
        try {
            inputStream = file.getInputStream();
            // 2.应用HUtool ExcelUtil获取ExcelReader指定输入流和sheet
            ExcelReader excelReader = ExcelUtil.getReader(inputStream);
            // 可以加上表头验证
            // 3.读取第二行到最后一行数据
            List<List<Object>> read = excelReader.read(1, excelReader.getRowCount());
            List<OrderBatchDeliverDTO> orderBatchDeliverDTOList=new ArrayList<>();
            for (List<Object> objects : read) {
                OrderBatchDeliverDTO orderBatchDeliverDTO=new OrderBatchDeliverDTO();
                orderBatchDeliverDTO.setOrderSn(objects.get(0).toString());
                orderBatchDeliverDTO.setLogisticsName(objects.get(1).toString());
                orderBatchDeliverDTO.setLogisticsNo(objects.get(2).toString());
                orderBatchDeliverDTOList.add(orderBatchDeliverDTO);
            }
            orderService.batchDeliver(orderBatchDeliverDTOList);
        } catch (Exception e) {
            e.printStackTrace();
        }

    }
}