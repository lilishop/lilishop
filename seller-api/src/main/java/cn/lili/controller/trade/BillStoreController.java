package cn.lili.controller.trade;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.order.order.entity.dos.StoreFlow;
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

/**
 * 店铺端,结算单接口
 *
 * @author Chopper
 * @date 2020/11/17 4:29 下午
 */
@RestController
@Api(tags = "店铺端,结算单接口")
@RequestMapping("/store/bill")
public class BillStoreController {

    @Autowired
    private BillService billService;

    @ApiOperation(value = "获取结算单分页")
    @GetMapping(value = "/getByPage")
    public ResultMessage<IPage<BillListVO>> getByPage(BillSearchParams billSearchParams) {
        return ResultUtil.data(billService.billPage(billSearchParams));
    }

    @ApiOperation(value = "通过id获取结算单")
    @ApiImplicitParam(name = "id", value = "结算单ID", required = true, paramType = "path", dataType = "String")
    @GetMapping(value = "/get/{id}")
    public ResultMessage<Bill> get(@PathVariable String id) {
        return ResultUtil.data(billService.getById(id));
    }

    @ApiOperation(value = "获取商家结算单流水分页")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "结算单ID", required = true, paramType = "path", dataType = "String"),
            @ApiImplicitParam(name = "flowType", value = "流水类型:PAY、REFUND", paramType = "query", dataType = "String")
    })
    @GetMapping(value = "/{id}/getStoreFlow")
    public ResultMessage<IPage<StoreFlow>> getStoreFlow(@PathVariable String id, String flowType, PageVO pageVO) {
        return ResultUtil.data(billService.getStoreFlow(id, flowType, pageVO));
    }

    @ApiOperation(value = "获取商家分销订单流水分页")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "结算单ID", required = true, paramType = "path", dataType = "String")
    })
    @GetMapping(value = "/{id}/getDistributionFlow")
    public ResultMessage<IPage<StoreFlow>> getDistributionFlow(@PathVariable String id, PageVO pageVO) {
        return ResultUtil.data(billService.getDistributionFlow(id, pageVO));
    }

    @ApiOperation(value = "核对结算单")
    @ApiImplicitParam(name = "id", value = "结算单ID", required = true, paramType = "path", dataType = "String")
    @PutMapping(value = "/check/{id}")
    public ResultMessage<Object> examine(@PathVariable String id) {
        billService.check(id);
        return ResultUtil.success();
    }
}
