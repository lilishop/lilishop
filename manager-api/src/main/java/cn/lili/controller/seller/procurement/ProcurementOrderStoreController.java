package cn.lili.controller.seller.procurement;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.procurement.entity.dos.ProcurementOrder;
import cn.lili.modules.procurement.entity.dto.AuditActionDTO;
import cn.lili.modules.procurement.entity.dto.ProcurementOrderCreateDTO;
import cn.lili.modules.procurement.entity.params.ProcurementOrderSearchParams;
import cn.lili.modules.procurement.entity.vos.ProcurementOrderVO;
import cn.lili.modules.procurement.service.ProcurementOrderService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * 店铺端采购单接口
 * 支持创建、提交、审核、关闭、详情与分页查询
 * @author Bulbasaur
 * @since 2025-12-18
 */
@Tag(name = "店铺端,采购单接口")
@RestController
@RequestMapping("/store/procurement/order")
public class ProcurementOrderStoreController {

    @Autowired
    private ProcurementOrderService procurementOrderService;

    @Operation(summary = "新建采购单")
    @PostMapping
    public ResultMessage<ProcurementOrderVO> create(@RequestBody ProcurementOrderCreateDTO dto) {
        return ResultUtil.data(procurementOrderService.create(dto));
    }

    @Operation(summary = "提交采购单")
    @Parameter(name = "id", description = "采购单ID", required = true)
    @PutMapping("/submit/{id}")
    public ResultMessage<Boolean> submit(@PathVariable String id) {
        return ResultUtil.data(procurementOrderService.submit(id));
    }

    @Operation(summary = "审核采购单")
    @Parameter(name = "id", description = "采购单ID", required = true)
    @PutMapping("/audit/{id}")
    public ResultMessage<Boolean> audit(@PathVariable String id, @RequestBody AuditActionDTO action) {
        return ResultUtil.data(procurementOrderService.audit(id, action));
    }

    @Operation(summary = "关闭采购单")
    @Parameter(name = "id", description = "采购单ID", required = true)
    @PutMapping("/close/{id}")
    public ResultMessage<Boolean> close(@PathVariable String id) {
        return ResultUtil.data(procurementOrderService.close(id));
    }

    @Operation(summary = "采购单各状态数量")
    @GetMapping("/status-count")
    public ResultMessage<Map<String, Long>> statusCount(ProcurementOrderSearchParams params) {
        return ResultUtil.data(procurementOrderService.statusCount(params));
    }

    @Operation(summary = "采购单详情")
    @Parameter(name = "id", description = "采购单ID", required = true)
    @GetMapping("/{id}")
    public ResultMessage<ProcurementOrderVO> detail(@PathVariable String id) {
        return ResultUtil.data(procurementOrderService.getDetail(id));
    }

    @Operation(summary = "采购单分页")
    @GetMapping
    public ResultMessage<IPage<ProcurementOrder>> page(ProcurementOrderSearchParams params) {
        return ResultUtil.data(procurementOrderService.page(params));
    }
}
