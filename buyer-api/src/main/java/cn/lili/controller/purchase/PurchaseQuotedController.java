package cn.lili.controller.purchase;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.purchase.entity.dos.PurchaseQuoted;
import cn.lili.modules.purchase.entity.vos.PurchaseQuotedVO;
import cn.lili.modules.purchase.service.PurchaseQuotedService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.constraints.NotNull;
import java.util.List;

/**
 * 买家端,采购报价接口
 *
 * @author Bulbasaur
 * @date: 2020/11/16 10:06 下午
 */
@Api(tags = "买家端,采购报价接口")
@RestController
@RequestMapping("/buyer/purchaseQuoted")
public class PurchaseQuotedController {

    /**
     * 采购单报价
     */
    @Autowired
    private PurchaseQuotedService purchaseQuotedService;

    @ApiOperation(value = "添加采购单报价")
    @PostMapping
    public ResultMessage<PurchaseQuoted> addPurchaseOrderVO(PurchaseQuotedVO purchaseQuotedVO) {
        return ResultUtil.data(purchaseQuotedService.addPurchaseQuoted(purchaseQuotedVO));
    }

    @ApiOperation(value = "报价列表")
    @ApiImplicitParam(name = "purchaseOrderId", value = "报价单ID", required = true, dataType = "String", paramType = "path")
    @GetMapping("/purchaseOrder/{purchaseOrderId}")
    public ResultMessage<List<PurchaseQuoted>> get(@NotNull @PathVariable String purchaseOrderId) {
        return ResultUtil.data(purchaseQuotedService.getByPurchaseOrderId(purchaseOrderId));
    }

    @ApiOperation(value = "报价单详情")
    @ApiImplicitParam(name = "id", value = "报价单ID", required = true, dataType = "String", paramType = "path")
    @GetMapping(value = "purchaseQuoted/{id}")
    public ResultMessage<PurchaseQuotedVO> getPurchaseQuoted(@NotNull @PathVariable String id) {
        return ResultUtil.data(purchaseQuotedService.getById(id));
    }


}
