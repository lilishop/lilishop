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
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.constraints.NotNull;
import java.util.Date;
import java.util.List;

/**
 * 买家端,售后管理接口
 *
 * @author Chopper
 * @since 2020/11/16 10:02 下午
 */
@RestController
@Tag(name = "买家端,售后管理接口")
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

    @Operation(summary = "查看售后服务详情")
    @Parameter(name = "sn", description = "售后单号", required = true)
    @GetMapping("/get/{sn}")
    public ResultMessage<AfterSaleVO> get(@NotNull(message = "售后单号") @PathVariable("sn") String sn) {
        AfterSaleVO afterSale = OperationalJudgment.judgment(afterSaleService.getAfterSale(sn));
        return ResultUtil.data(afterSale);
    }

    @Operation(summary = "分页获取售后服务")
    @Parameter(name = "searchParams", description = "售后查询参数")
    @GetMapping("/page")
    public ResultMessage<IPage<AfterSaleVO>> getByPage(AfterSaleSearchParams searchParams) {
        return ResultUtil.data(afterSaleService.getAfterSalePages(searchParams));
    }

    @Operation(summary = "获取申请售后页面信息")
    @Parameter(name = "sn", description = "订单货物编号", required = true)
    @GetMapping("/applyAfterSaleInfo/{sn}")
    public ResultMessage<AfterSaleApplyVO> applyAfterSaleInfo(@PathVariable String sn) {
        return ResultUtil.data(afterSaleService.getAfterSaleVO(sn));
    }

    @PreventDuplicateSubmissions
    @Operation(summary = "申请售后")
    @Parameter(name = "orderItemSn", description = "订单货物编号", required = true)
    @PostMapping("/save/{orderItemSn}")
    public ResultMessage<AfterSale> save(@Validated AfterSaleDTO afterSaleDTO) {
        return ResultUtil.data(afterSaleService.saveAfterSale(afterSaleDTO));

    }

    @Operation(summary = "买家 退回 物流信息")
    @Parameter(name = "afterSaleSn", description = "售后sn", required = true)
    @Parameter(name = "logisticsNo", description = "发货单号", required = true)
    @Parameter(name = "logisticsId", description = "物流公司id", required = true)
    @Parameter(name = "mDeliverTime", description = "买家发货时间", required = true)
    @PostMapping("/delivery/{afterSaleSn}")
    public ResultMessage<AfterSale> delivery(@NotNull(message = "售后编号不能为空") @PathVariable("afterSaleSn") String afterSaleSn,
                                             @NotNull(message = "发货单号不能为空") @RequestParam String logisticsNo,
                                             @NotNull(message = "请选择物流公司") @RequestParam String logisticsId,
                                             @NotNull(message = "请选择发货时间") @RequestParam @DateTimeFormat(pattern = "yyyy-MM-dd") Date mDeliverTime) {
        return ResultUtil.data(afterSaleService.buyerDelivery(afterSaleSn, logisticsNo, logisticsId, mDeliverTime));
    }

    @PreventDuplicateSubmissions
    @Operation(summary = "售后，取消售后")  
    @Parameter(name = "afterSaleSn", description = "售后sn", required = true)
    @PostMapping("/cancel/{afterSaleSn}")
    public ResultMessage<AfterSale> cancel(@NotNull(message = "参数非法") @PathVariable("afterSaleSn") String afterSaleSn) {
        return ResultUtil.data(afterSaleService.cancel(afterSaleSn));
    }

    @Operation(summary = "获取商家售后收件地址")
    @Parameter(name = "sn", description = "售后单号", required = true)
    @GetMapping("/getStoreAfterSaleAddress/{sn}")
    public ResultMessage<StoreAfterSaleAddressDTO> getStoreAfterSaleAddress(@NotNull(message = "售后单号") @PathVariable("sn") String sn) {
        return ResultUtil.data(afterSaleService.getStoreAfterSaleAddressDTO(sn));
    }

    @Operation(summary = "获取售后原因")
    @Parameter(name = "serviceType", description = "售后类型", required = true)
    @GetMapping("/get/afterSaleReason/{serviceType}")
    public ResultMessage<List<AfterSaleReason>> getAfterSaleReason(@PathVariable String serviceType) {
        return ResultUtil.data(afterSaleReasonService.afterSaleReasonList(serviceType));
    }

    @Operation(summary = "获取售后日志")
    @Parameter(name = "sn", description = "售后编号", required = true)
    @GetMapping("/get/getAfterSaleLog/{sn}")
    public ResultMessage<List<AfterSaleLog>> getAfterSaleLog(@PathVariable String sn) {
        // 安全校验：核验当前售后单归属权，防止买家通过遍历 sn 水平越权窃取他人售后流转日志
        OperationalJudgment.judgment(afterSaleService.getAfterSale(sn));
        return ResultUtil.data(afterSaleLogService.getAfterSaleLog(sn));
    }

}
