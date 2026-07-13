package cn.lili.controller.seller.order;

import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.order.aftersale.entity.dos.AfterSale;
import cn.lili.modules.order.aftersale.entity.vo.AfterSaleNumVO;
import cn.lili.modules.order.aftersale.entity.vo.AfterSaleSearchParams;
import cn.lili.modules.order.aftersale.entity.vo.AfterSaleVO;
import cn.lili.modules.order.aftersale.service.AfterSaleService;
import cn.lili.modules.store.entity.dto.StoreAfterSaleAddressDTO;
import cn.lili.modules.system.entity.vo.Traces;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.Parameter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.constraints.NotNull;
import java.util.List;
import java.util.Objects;

/**
 * 店铺端,售后管理接口
 *
 * @author Chopper
 * @since 2020/11/17 4:29 下午
 */
@RestController
@Tag(name = "店铺端,售后管理接口")
@RequestMapping("/store/order/afterSale")
public class AfterSaleStoreController {

    @Autowired
    private AfterSaleService afterSaleService;

    @Operation(summary = "查看售后服务详情")
    @Parameter(name = "sn", description = "售后单号", required = true)
    @GetMapping("/{sn}")
    public ResultMessage<AfterSaleVO> get(@PathVariable String sn) {
        AfterSaleVO afterSale = OperationalJudgment.judgment(afterSaleService.getAfterSale(sn));
        return ResultUtil.data(afterSale);
    }

    @Operation(summary = "分页获取售后服务")
    @Parameter(name = "afterSaleSearchParams", description = "售后查询参数")
    @GetMapping("/page")
    public ResultMessage<IPage<AfterSaleVO>> getByPage(AfterSaleSearchParams searchParams) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        searchParams.setStoreId(storeId);
        return ResultUtil.data(afterSaleService.getAfterSalePages(searchParams));
    }

    @Operation(summary = "获取售后数量")
    @Parameter(name = "afterSaleSearchParams", description = "售后查询参数")
    @GetMapping("/afterSaleNumVO")
    public ResultMessage<AfterSaleNumVO> getAfterSaleNumVO(AfterSaleSearchParams afterSaleSearchParams) {
        return ResultUtil.data(afterSaleService.getAfterSaleNumVO(afterSaleSearchParams));
    }

    @Operation(summary = "获取导出售后服务列表列表")
    @Parameter(name = "afterSaleSearchParams", description = "售后查询参数")
    @GetMapping("/exportAfterSaleOrder")
    public ResultMessage<List<AfterSale>> exportAfterSaleOrder(AfterSaleSearchParams searchParams) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        searchParams.setStoreId(storeId);
        return ResultUtil.data(afterSaleService.exportAfterSaleOrder(searchParams));
    }

    @PreventDuplicateSubmissions
    @Operation(summary = "审核售后申请")
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

            

    @PreventDuplicateSubmissions
    @Operation(summary = "卖家确认收货")
    @Parameter(name = "afterSaleSn", description = "售后sn", required = true)
    @Parameter(name = "serviceStatus", description = "PASS：审核通过，REFUSE：审核未通过", required = true)
    @Parameter(name = "remark", description = "备注")
    @PutMapping("/confirm/{afterSaleSn}")
    public ResultMessage<AfterSale> confirm(@NotNull(message = "请选择售后单") @PathVariable String afterSaleSn,
                                            @NotNull(message = "请审核") String serviceStatus,
                                            String remark) {
        return ResultUtil.data(afterSaleService.storeConfirm(afterSaleSn, serviceStatus, remark));
    }

    @Operation(summary = "查看买家退货物流踪迹")
    @Parameter(name = "sn", description = "售后单号", required = true)
    @GetMapping("/getDeliveryTraces/{sn}")
    public ResultMessage<Traces> getDeliveryTraces(@PathVariable String sn) {
        return ResultUtil.data(afterSaleService.deliveryTraces(sn));
    }

    @Operation(summary = "获取商家售后收件地址")
    @Parameter(name = "sn", description = "售后单号", required = true)
    @GetMapping("/getStoreAfterSaleAddress/{sn}")
    public ResultMessage<StoreAfterSaleAddressDTO> getStoreAfterSaleAddress(@NotNull(message = "售后单号") @PathVariable("sn") String sn) {
        return ResultUtil.data(afterSaleService.getStoreAfterSaleAddressDTO(sn));
    }

}
