package cn.lili.controller.manager.order;

import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.order.aftersale.entity.dos.AfterSale;
import cn.lili.modules.order.aftersale.entity.vo.AfterSaleNumVO;
import cn.lili.modules.order.aftersale.entity.vo.AfterSaleSearchParams;
import cn.lili.modules.order.aftersale.entity.vo.AfterSaleVO;
import cn.lili.modules.order.aftersale.service.AfterSaleService;
import cn.lili.modules.store.entity.dto.StoreAfterSaleAddressDTO;
import cn.lili.modules.system.entity.vo.Traces;
import com.baomidou.mybatisplus.core.metadata.IPage;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;

import jakarta.validation.constraints.NotNull;
import java.util.List;

/**
 * 管理端,售后接口
 *
 * @author Bulbasaur
 * @since 2021/1/6 14:11
 */
@RestController
@RequestMapping("/manager/order/afterSale")
@Tag(name = "管理端,售后接口")
public class AfterSaleManagerController {

    /**
     * 售后
     */
    @Autowired
    private AfterSaleService afterSaleService;

    @Operation(summary = "分页获取售后服务")
    @Parameter(name = "searchParams", description = "售后查询参数")
    @GetMapping("/page")
    public ResultMessage<IPage<AfterSaleVO>> getByPage(AfterSaleSearchParams searchParams) {
        return ResultUtil.data(afterSaleService.getAfterSalePages(searchParams));
    }

    @Operation(summary = "获取售后数量")
    @Parameter(name = "afterSaleSearchParams", description = "售后查询参数")
    @GetMapping("/afterSaleNumVO")
    public ResultMessage<AfterSaleNumVO> getAfterSaleNumVO(AfterSaleSearchParams afterSaleSearchParams) {
        return ResultUtil.data(afterSaleService.getAfterSaleNumVO(afterSaleSearchParams));
    }

    @Operation(summary = "获取导出售后服务列表列表")
    @Parameter(name = "searchParams", description = "售后查询参数")
    @GetMapping("/exportAfterSaleOrder")
    public ResultMessage<List<AfterSale>> exportAfterSaleOrder(AfterSaleSearchParams searchParams) {
        return ResultUtil.data(afterSaleService.exportAfterSaleOrder(searchParams));
    }

    @Operation(summary = "查看售后服务详情")
    @Parameter(name = "sn", description = "售后单号", required = true)
    @GetMapping("/get/{sn}")
    public ResultMessage<AfterSaleVO> get(@NotNull(message = "售后单号") @PathVariable("sn") String sn) {
        return ResultUtil.data(afterSaleService.getAfterSale(sn));
    }

    @Operation(summary = "查看买家退货物流踪迹")
    @Parameter(name = "sn", description = "售后单号", required = true)
    @GetMapping("/getDeliveryTraces/{sn}")
    public ResultMessage<Traces> getDeliveryTraces(@PathVariable String sn) {
        return ResultUtil.data(afterSaleService.deliveryTraces(sn));
    }

    @PreventDuplicateSubmissions
    @Operation(description = "售后线下退款")
    @Parameter(name = "afterSaleSn", description = "售后sn", required = true)
    @Parameter(name = "remark", description = "备注")
    @PutMapping("/refund/{afterSaleSn}")
    public ResultMessage<AfterSale> refund(@NotNull(message = "请选择售后单") @PathVariable String afterSaleSn,
                                           @RequestParam String remark) {

        return ResultUtil.data(afterSaleService.refund(afterSaleSn, remark));
    }

    @PreventDuplicateSubmissions
    @Operation(description = "审核售后申请")
    @Parameter(name = "afterSaleSn", description = "售后sn", required = true)
    @Parameter(name = "serviceStatus", description = "PASS：审核通过，REFUSE：审核未通过", required = true)
    @Parameter(name = "remark", description = "备注")
    @Parameter(name = "actualRefundPrice", description = "实际退款金额")
    @PutMapping("/review/{afterSaleSn}")
    public ResultMessage<AfterSale> review(@NotNull(message = "请选择售后单") @PathVariable String afterSaleSn,
                                           @NotNull(message = "请审核") String serviceStatus,
                                           String remark,
                                           Double actualRefundPrice) {

        return ResultUtil.data(afterSaleService.review(afterSaleSn, serviceStatus, remark,actualRefundPrice));
    }

    @Operation(description = "获取商家售后收件地址")
    @Parameter(name = "sn", description = "售后单号", required = true)
    @GetMapping("/getStoreAfterSaleAddress/{sn}")
    public ResultMessage<StoreAfterSaleAddressDTO> getStoreAfterSaleAddress(@NotNull(message = "售后单号") @PathVariable("sn") String sn) {
        return ResultUtil.data(afterSaleService.getStoreAfterSaleAddressDTO(sn));
    }
}
