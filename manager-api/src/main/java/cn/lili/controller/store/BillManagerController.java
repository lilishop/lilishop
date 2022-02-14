package cn.lili.controller.store;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.order.order.entity.dos.StoreFlow;
import cn.lili.modules.order.order.service.StoreFlowService;
import cn.lili.modules.store.entity.dos.Bill;
import cn.lili.modules.store.entity.dto.BillSearchParams;
import cn.lili.modules.store.entity.vos.BillListVO;
import cn.lili.modules.store.service.BillService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.constraints.NotNull;

/**
 * 管理端,商家结算单接口
 *
 * @author Chopper
 * @since 2020/11/17 7:23 下午
 */
@RestController
@Api(tags = "管理端,商家结算单接口")
@RequestMapping("/manager/order/bill")
public class BillManagerController {
    @Autowired
    private BillService billService;

    @Autowired
    private StoreFlowService storeFlowService;

    @ApiOperation(value = "通过id获取结算单")
    @ApiImplicitParam(name = "id", value = "结算单ID", required = true, paramType = "path")
    @GetMapping(value = "/get/{id}")
    public ResultMessage<Bill> get(@PathVariable @NotNull String id) {
        return ResultUtil.data(billService.getById(id));
    }

    @ApiOperation(value = "获取结算单分页")
    @GetMapping(value = "/getByPage")
    public ResultMessage<IPage<BillListVO>> getByPage(BillSearchParams billSearchParams) {
        return ResultUtil.data(billService.billPage(billSearchParams));
    }

    @ApiOperation(value = "获取商家结算单流水分页")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "结算单ID", required = true, paramType = "path"),
            @ApiImplicitParam(name = "flowType", value = "流水类型:PAY、REFUND", paramType = "query")
    })
    @GetMapping(value = "/{id}/getStoreFlow")
    public ResultMessage<IPage<StoreFlow>> getStoreFlow(@PathVariable String id, String flowType, PageVO pageVO) {
        return ResultUtil.data(storeFlowService.getStoreFlow(id, flowType, pageVO));
    }

    @ApiOperation(value = "支付结算单")
    @ApiImplicitParam(name = "id", value = "结算单ID", required = true, paramType = "path")
    @PutMapping(value = "/pay/{id}")
    public ResultMessage<Object> pay(@PathVariable String id) {
        billService.complete(id);
        return ResultUtil.success();
    }

}
