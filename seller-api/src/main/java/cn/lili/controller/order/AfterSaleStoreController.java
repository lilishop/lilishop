package cn.lili.controller.order;

import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.security.context.UserContext;
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
import java.util.Objects;

/**
 * 店铺端,售后管理接口
 *
 * @author Chopper
 * @since 2020/11/17 4:29 下午
 */
@RestController
@Api(tags = "店铺端,售后管理接口")
@RequestMapping("/store/afterSale")
public class AfterSaleStoreController {

    @Autowired
    private AfterSaleService afterSaleService;

    @ApiOperation(value = "查看售后服务详情")
    @ApiImplicitParam(name = "sn", value = "售后单号", required = true, paramType = "path")
    @GetMapping(value = "/{sn}")
    public ResultMessage<AfterSaleVO> get(@PathVariable String sn) {
        AfterSaleVO afterSale = OperationalJudgment.judgment(afterSaleService.getAfterSale(sn));
        return ResultUtil.data(afterSale);
    }

    @ApiOperation(value = "分页获取售后服务")
    @GetMapping(value = "/page")
    public ResultMessage<IPage<AfterSaleVO>> getByPage(AfterSaleSearchParams searchParams) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        searchParams.setStoreId(storeId);
        return ResultUtil.data(afterSaleService.getAfterSalePages(searchParams));
    }

    @ApiOperation(value = "获取导出售后服务列表列表")
    @GetMapping(value = "/exportAfterSaleOrder")
    public ResultMessage<List<AfterSale>> exportAfterSaleOrder(AfterSaleSearchParams searchParams) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        searchParams.setStoreId(storeId);
        return ResultUtil.data(afterSaleService.exportAfterSaleOrder(searchParams));
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

    @PreventDuplicateSubmissions
    @ApiOperation(value = "卖家确认收货")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "afterSaleSn", value = "售后sn", required = true, paramType = "path"),
            @ApiImplicitParam(name = "serviceStatus", value = "PASS：审核通过，REFUSE：审核未通过", required = true, paramType = "query"),
            @ApiImplicitParam(name = "remark", value = "备注", paramType = "query")
    })
    @PutMapping(value = "/confirm/{afterSaleSn}")
    public ResultMessage<AfterSale> confirm(@NotNull(message = "请选择售后单") @PathVariable String afterSaleSn,
                                            @NotNull(message = "请审核") String serviceStatus,
                                            String remark) {

        return ResultUtil.data(afterSaleService.storeConfirm(afterSaleSn, serviceStatus, remark));
    }

    @ApiOperation(value = "查看买家退货物流踪迹")
    @ApiImplicitParam(name = "sn", value = "售后单号", required = true, paramType = "path")
    @GetMapping(value = "/getDeliveryTraces/{sn}")
    public ResultMessage<Traces> getDeliveryTraces(@PathVariable String sn) {
        return ResultUtil.data(afterSaleService.deliveryTraces(sn));
    }

    @ApiOperation(value = "获取商家售后收件地址")
    @ApiImplicitParam(name = "sn", value = "售后单号", required = true, paramType = "path")
    @GetMapping(value = "/getStoreAfterSaleAddress/{sn}")
    public ResultMessage<StoreAfterSaleAddressDTO> getStoreAfterSaleAddress(@NotNull(message = "售后单号") @PathVariable("sn") String sn) {
        return ResultUtil.data(afterSaleService.getStoreAfterSaleAddressDTO(sn));
    }

}
