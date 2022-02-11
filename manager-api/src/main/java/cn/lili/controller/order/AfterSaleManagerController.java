package cn.lili.controller.order;

import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.order.aftersale.entity.dos.AfterSale;
import cn.lili.modules.order.aftersale.entity.vo.AfterSaleSearchParams;
import cn.lili.modules.order.aftersale.entity.vo.AfterSaleVO;
import cn.lili.modules.order.aftersale.service.AfterSaleService;
import cn.lili.modules.store.entity.dto.StoreAfterSaleAddressDTO;
import cn.lili.modules.system.entity.vo.Traces;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.constraints.NotNull;
import java.util.List;

/**
 * 管理端,售后接口
 *
 * @author Bulbasaur
 * @since 2021/1/6 14:11
 */
@RestController
@RequestMapping("/manager/afterSale")
@Api(tags = "管理端,售后接口")
public class AfterSaleManagerController {

    /**
     * 售后
     */
    @Autowired
    private AfterSaleService afterSaleService;

    @ApiOperation(value = "分页获取售后服务")
    @GetMapping(value = "/page")
    public ResultMessage<IPage<AfterSaleVO>> getByPage(AfterSaleSearchParams searchParams) {
        return ResultUtil.data(afterSaleService.getAfterSalePages(searchParams));
    }

    @ApiOperation(value = "获取导出售后服务列表列表")
    @GetMapping(value = "/exportAfterSaleOrder")
    public ResultMessage<List<AfterSale>> exportAfterSaleOrder(AfterSaleSearchParams searchParams) {
        return ResultUtil.data(afterSaleService.exportAfterSaleOrder(searchParams));
    }

    @ApiOperation(value = "查看售后服务详情")
    @ApiImplicitParam(name = "sn", value = "售后单号", required = true, paramType = "path")
    @GetMapping(value = "/get/{sn}")
    public ResultMessage<AfterSaleVO> get(@NotNull(message = "售后单号") @PathVariable("sn") String sn) {
        return ResultUtil.data(afterSaleService.getAfterSale(sn));
    }

    @ApiOperation(value = "查看买家退货物流踪迹")
    @ApiImplicitParam(name = "sn", value = "售后单号", required = true, paramType = "path")
    @GetMapping(value = "/getDeliveryTraces/{sn}")
    public ResultMessage<Traces> getDeliveryTraces(@PathVariable String sn) {
        return ResultUtil.data(afterSaleService.deliveryTraces(sn));
    }

    @ApiOperation(value = "售后线下退款")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "afterSaleSn", value = "售后sn", required = true, paramType = "path"),
            @ApiImplicitParam(name = "remark", value = "备注", paramType = "query")
    })
    @PutMapping(value = "/refund/{afterSaleSn}")
    public ResultMessage<AfterSale> refund(@NotNull(message = "请选择售后单") @PathVariable String afterSaleSn,
                                           @RequestParam String remark) {

        return ResultUtil.data(afterSaleService.refund(afterSaleSn, remark));
    }

    @PreventDuplicateSubmissions
    @ApiOperation(value = "审核售后申请")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "afterSaleSn", value = "售后sn", required = true, paramType = "path"),
            @ApiImplicitParam(name = "serviceStatus", value = "PASS：审核通过，REFUSE：审核未通过", required = true, paramType = "query"),
            @ApiImplicitParam(name = "remark", value = "备注", paramType = "query"),
            @ApiImplicitParam(name = "actualRefundPrice", value = "实际退款金额", paramType = "query")
    })
    @PutMapping(value = "/review/{afterSaleSn}")
    public ResultMessage<AfterSale> review(@NotNull(message = "请选择售后单") @PathVariable String afterSaleSn,
                                           @NotNull(message = "请审核") String serviceStatus,
                                           String remark,
                                           Double actualRefundPrice) {

        return ResultUtil.data(afterSaleService.review(afterSaleSn, serviceStatus, remark,actualRefundPrice));
    }

    @ApiOperation(value = "获取商家售后收件地址")
    @ApiImplicitParam(name = "sn", value = "售后单号", required = true, paramType = "path")
    @GetMapping(value = "/getStoreAfterSaleAddress/{sn}")
    public ResultMessage<StoreAfterSaleAddressDTO> getStoreAfterSaleAddress(@NotNull(message = "售后单号") @PathVariable("sn") String sn) {
        return ResultUtil.data(afterSaleService.getStoreAfterSaleAddressDTO(sn));
    }
}
