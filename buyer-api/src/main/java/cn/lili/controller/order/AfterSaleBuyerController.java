package cn.lili.controller.order;

import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.order.aftersale.entity.dos.AfterSale;
import cn.lili.modules.order.aftersale.entity.dos.AfterSaleLog;
import cn.lili.modules.order.aftersale.entity.dos.AfterSaleReason;
import cn.lili.modules.order.aftersale.entity.dto.AfterSaleDTO;
import cn.lili.modules.order.aftersale.entity.vo.AfterSaleApplyVO;
import cn.lili.modules.order.aftersale.entity.vo.AfterSaleSearchParams;
import cn.lili.modules.order.aftersale.entity.vo.AfterSaleVO;
import cn.lili.modules.order.aftersale.service.AfterSaleLogService;
import cn.lili.modules.order.aftersale.service.AfterSaleReasonService;
import cn.lili.modules.order.aftersale.service.AfterSaleService;
import cn.lili.modules.store.entity.dto.StoreAfterSaleAddressDTO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.web.bind.annotation.*;

import javax.validation.constraints.NotNull;
import java.util.Date;
import java.util.List;

/**
 * 买家端,售后管理接口
 *
 * @author Chopper
 * @since 2020/11/16 10:02 下午
 */
@RestController
@Api(tags = "买家端,售后管理接口")
@RequestMapping("/buyer/order/afterSale")
public class AfterSaleBuyerController {

    /**
     * 售后
     */
    @Autowired
    private AfterSaleService afterSaleService;
    /**
     * 售后原因
     */
    @Autowired
    private AfterSaleReasonService afterSaleReasonService;
    /**
     * 售后日志
     */
    @Autowired
    private AfterSaleLogService afterSaleLogService;

    @ApiOperation(value = "查看售后服务详情")
    @ApiImplicitParam(name = "sn", value = "售后单号", required = true, paramType = "path")
    @GetMapping(value = "/get/{sn}")
    public ResultMessage<AfterSaleVO> get(@NotNull(message = "售后单号") @PathVariable("sn") String sn) {
        AfterSaleVO afterSale = OperationalJudgment.judgment(afterSaleService.getAfterSale(sn));
        return ResultUtil.data(afterSale);
    }

    @ApiOperation(value = "分页获取售后服务")
    @GetMapping(value = "/page")
    public ResultMessage<IPage<AfterSaleVO>> getByPage(AfterSaleSearchParams searchParams) {
        return ResultUtil.data(afterSaleService.getAfterSalePages(searchParams));
    }

    @ApiOperation(value = "获取申请售后页面信息")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "sn", value = "订单货物编号", required = true, dataType = "String", paramType = "path")
    })
    @GetMapping(value = "/applyAfterSaleInfo/{sn}")
    public ResultMessage<AfterSaleApplyVO> applyAfterSaleInfo(@PathVariable String sn) {
        return ResultUtil.data(afterSaleService.getAfterSaleVO(sn));
    }

    @PreventDuplicateSubmissions
    @PostMapping(value = "/save/{orderItemSn}")
    @ApiImplicitParam(name = "orderItemSn", value = "订单货物编号", required = true, paramType = "query")
    @ApiOperation(value = "申请售后")
    public ResultMessage<AfterSale> save(AfterSaleDTO afterSaleDTO) {
        return ResultUtil.data(afterSaleService.saveAfterSale(afterSaleDTO));

    }

    @ApiOperation(value = "买家 退回 物流信息")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "afterSaleSn", value = "售后sn", required = true, dataType = "String", paramType = "path"),
            @ApiImplicitParam(name = "logisticsNo", value = "发货单号", required = true, dataType = "String", paramType = "query"),
            @ApiImplicitParam(name = "logisticsId", value = "物流公司id", required = true, dataType = "String", paramType = "query"),
            @ApiImplicitParam(name = "mDeliverTime", value = "买家发货时间", required = true, dataType = "date", paramType = "query")

    })
    @PostMapping(value = "/delivery/{afterSaleSn}")
    public ResultMessage<AfterSale> delivery(@NotNull(message = "售后编号不能为空") @PathVariable("afterSaleSn") String afterSaleSn,
                                             @NotNull(message = "发货单号不能为空") @RequestParam String logisticsNo,
                                             @NotNull(message = "请选择物流公司") @RequestParam String logisticsId,
                                             @NotNull(message = "请选择发货时间") @RequestParam @DateTimeFormat(pattern = "yyyy-MM-dd") Date mDeliverTime) {
        return ResultUtil.data(afterSaleService.buyerDelivery(afterSaleSn, logisticsNo, logisticsId, mDeliverTime));
    }

    @PreventDuplicateSubmissions
    @ApiOperation(value = "售后，取消售后")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "afterSaleSn", value = "售后sn", required = true, dataType = "String", paramType = "path")
    })
    @PostMapping(value = "/cancel/{afterSaleSn}")
    public ResultMessage<AfterSale> cancel(@NotNull(message = "参数非法") @PathVariable("afterSaleSn") String afterSaleSn) {
        return ResultUtil.data(afterSaleService.cancel(afterSaleSn));
    }

    @ApiOperation(value = "获取商家售后收件地址")
    @ApiImplicitParam(name = "sn", value = "售后单号", required = true, paramType = "path")
    @GetMapping(value = "/getStoreAfterSaleAddress/{sn}")
    public ResultMessage<StoreAfterSaleAddressDTO> getStoreAfterSaleAddress(@NotNull(message = "售后单号") @PathVariable("sn") String sn) {
        return ResultUtil.data(afterSaleService.getStoreAfterSaleAddressDTO(sn));
    }

    @ApiOperation(value = "获取售后原因")
    @ApiImplicitParam(name = "serviceType", value = "售后类型", required = true, paramType = "path", allowableValues = "CANCEL,RETURN_GOODS,RETURN_MONEY,COMPLAIN")
    @GetMapping(value = "/get/afterSaleReason/{serviceType}")
    public ResultMessage<List<AfterSaleReason>> getAfterSaleReason(@PathVariable String serviceType) {
        return ResultUtil.data(afterSaleReasonService.afterSaleReasonList(serviceType));
    }

    @ApiOperation(value = "获取售后日志")
    @ApiImplicitParam(name = "sn", value = "售后编号", required = true, paramType = "path")
    @GetMapping(value = "/get/getAfterSaleLog/{sn}")
    public ResultMessage<List<AfterSaleLog>> getAfterSaleLog(@PathVariable String sn) {
        return ResultUtil.data(afterSaleLogService.getAfterSaleLog(sn));
    }

}
