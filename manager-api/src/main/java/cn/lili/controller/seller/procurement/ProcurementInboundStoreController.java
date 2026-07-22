package cn.lili.controller.seller.procurement;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.modules.procurement.entity.dos.ProcurementInbound;
import cn.lili.modules.procurement.entity.dos.ProcurementInboundItem;
import cn.lili.modules.procurement.entity.dto.ProcurementInboundCreateDTO;
import cn.lili.modules.procurement.entity.params.ProcurementInboundSearchParams;
import cn.lili.modules.procurement.service.ProcurementInboundItemService;
import cn.lili.modules.procurement.service.ProcurementInboundService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 店铺端采购入库接口
 * 支持创建入库单、分页与详情查询
 * @author Bulbasaur
 * @since 2025-12-18
 */
@Tag(name = "店铺端,采购入库接口")
@RestController
@RequestMapping("/store/procurement/inbound")
public class ProcurementInboundStoreController {

    @Autowired
    private ProcurementInboundService procurementInboundService;
    @Autowired
    private ProcurementInboundItemService procurementInboundItemService;

    @Operation(summary = "创建入库单")
    @PostMapping
    public ResultMessage<ProcurementInbound> create(@RequestBody ProcurementInboundCreateDTO dto) {
        return ResultUtil.data(procurementInboundService.createInbound(dto));
    }

    @Operation(summary = "入库单分页")
    @GetMapping
    public ResultMessage<IPage<ProcurementInbound>> page(ProcurementInboundSearchParams params) {
        return ResultUtil.data(procurementInboundService.page(params));
    }

    @Operation(summary = "入库单详情")
    @Parameter(name = "id", description = "入库单ID", required = true)
    @GetMapping("/{id}")
    public ResultMessage<ProcurementInbound> detail(@PathVariable String id) {
        return ResultUtil.data(procurementInboundService.getDetail(id));
    }

    @Operation(summary = "入库单明细列表")
    @Parameter(name = "id", description = "入库单ID", required = true)
    @GetMapping("/{id}/items")
    public ResultMessage<List<ProcurementInboundItem>> items(@PathVariable String id) {
        ProcurementInbound inbound = OperationalJudgment.judgment(procurementInboundService.getById(id));
        return ResultUtil.data(procurementInboundItemService.list(Wrappers.<ProcurementInboundItem>lambdaQuery()
                .eq(ProcurementInboundItem::getInboundId, inbound.getId())));
    }
}
