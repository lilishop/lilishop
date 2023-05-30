package cn.lili.controller.other.purchase;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.purchase.entity.dos.PurchaseOrder;
import cn.lili.modules.purchase.entity.dos.PurchaseQuoted;
import cn.lili.modules.purchase.entity.params.PurchaseOrderSearchParams;
import cn.lili.modules.purchase.entity.vos.PurchaseOrderVO;
import cn.lili.modules.purchase.entity.vos.PurchaseQuotedVO;
import cn.lili.modules.purchase.service.PurchaseOrderService;
import cn.lili.modules.purchase.service.PurchaseQuotedService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.constraints.NotNull;
import java.util.List;

/**
 * 管理端,采购接口
 *
 * @author Chopper
 * @since 2020/11/16 10:06 下午
 */
@Api(tags = "管理端,采购接口")
@RestController
@RequestMapping("/manager/other/purchase/purchase")
public class PurchaseManagerController {

    /**
     * 采购单
     */
    @Autowired
    private PurchaseOrderService purchaseOrderService;
    /**
     * 采购单报价
     */
    @Autowired
    private PurchaseQuotedService purchaseQuotedService;


    @ApiOperation(value = "采购单分页")
    @GetMapping
    public ResultMessage<IPage<PurchaseOrder>> get(PurchaseOrderSearchParams purchaseOrderSearchParams) {
        return ResultUtil.data(purchaseOrderService.page(purchaseOrderSearchParams));
    }

    @ApiOperation(value = "采购单详情")
    @ApiImplicitParam(name = "id", value = "采购单ID", required = true, dataType = "Long", paramType = "path")
    @GetMapping("/{id}")
    public ResultMessage<PurchaseOrderVO> getPurchaseOrder(@NotNull @PathVariable String id) {
        return ResultUtil.data(purchaseOrderService.getPurchaseOrder(id));
    }

    @ApiOperation(value = "关闭采购单")
    @ApiImplicitParam(name = "id", value = "采购单ID", required = true, dataType = "Long", paramType = "path")
    @PutMapping("/{id}")
    public ResultMessage<Object> close(@NotNull @PathVariable String id) {

        purchaseOrderService.close(id);
        return ResultUtil.success();
    }

    @ApiOperation(value = "报价列表")
    @ApiImplicitParam(name = "purchaseOrderId", value = "采购单ID", required = true, dataType = "String", paramType = "path")
    @GetMapping("/purchaseQuoted/list/{purchaseOrderId}")
    public ResultMessage<List<PurchaseQuoted>> get(@NotNull @PathVariable String purchaseOrderId) {
        return ResultUtil.data(purchaseQuotedService.getByPurchaseOrderId(purchaseOrderId));
    }

    @ApiOperation(value = "报价单详情")
    @ApiImplicitParam(name = "id", value = "报价单ID", required = true, dataType = "String", paramType = "path")
    @GetMapping("/purchaseQuoted/{id}")
    public ResultMessage<PurchaseQuotedVO> getPurchaseQuoted(@NotNull @PathVariable String id) {
        return ResultUtil.data(purchaseQuotedService.getById(id));
    }

}
