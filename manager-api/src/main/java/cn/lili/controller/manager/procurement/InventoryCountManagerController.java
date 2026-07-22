package cn.lili.controller.manager.procurement;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.context.ThreadContextHolder;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.procurement.entity.dos.InventoryCount;
import cn.lili.modules.procurement.entity.dos.InventoryCountItem;
import cn.lili.modules.procurement.service.InventoryCountItemService;
import cn.lili.modules.procurement.service.InventoryCountService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * 管理端,盘点单接口
 * 提供盘点单分页、详情、明细分页与下载
 *
 * @author Bulbasaur
 * @since 2025-12-18
 */
@Tag(name = "管理端,盘点单接口")
@RestController
@RequestMapping("/manager/procurement/inventory-count")
public class InventoryCountManagerController {

    @Autowired
    private InventoryCountService inventoryCountService;
    @Autowired
    private InventoryCountItemService inventoryCountItemService;

    @Operation(summary = "盘点单分页列表")
    @GetMapping("/page")
    public ResultMessage<IPage<InventoryCount>> page(PageVO pageVO) {
        return ResultUtil.data(inventoryCountService.page(pageVO));
    }

    @Operation(summary = "获取盘点单详情")
    @Parameter(name = "id", description = "盘点单ID", required = true)
    @GetMapping("/{id}")
    public ResultMessage<InventoryCount> get(@PathVariable String id) {
        return ResultUtil.data(OperationalJudgment.judgment(inventoryCountService.getById(id)));
    }

    @Operation(summary = "分页查看盘点单明细")
    @Parameter(name = "id", description = "盘点单ID", required = true)
    @GetMapping("/{id}/items/page")
    public ResultMessage<IPage<InventoryCountItem>> itemsPage(
            @PathVariable String id,
            PageVO pageVO,
            @RequestParam(required = false) String goodsName) {
        InventoryCount count = OperationalJudgment.judgment(inventoryCountService.getById(id));
        Page<InventoryCountItem> page = new Page<>(pageVO.getPageNumber(), pageVO.getPageSize());
        return ResultUtil.data(inventoryCountItemService.pageByCountId(count.getId(), page, goodsName));
    }

    @Operation(summary = "下载盘点单明细")
    @Parameter(name = "id", description = "盘点单ID", required = true)
    @GetMapping("/{id}/download")
    public ResultMessage<List<InventoryCountItem>> download(@PathVariable String id) {
        InventoryCount count = OperationalJudgment.judgment(inventoryCountService.getById(id));
        return ResultUtil.data(inventoryCountItemService.listByCountId(count.getId()));
    }

    @Operation(summary = "导出盘点单明细 Excel")
    @Parameter(name = "id", description = "盘点单ID", required = true)
    @GetMapping("/{id}/export")
    public void export(@PathVariable String id) {
        InventoryCount count = OperationalJudgment.judgment(inventoryCountService.getById(id));
        inventoryCountItemService.exportExcel(count.getId(), count.getSn(), ThreadContextHolder.getHttpResponse());
    }
}
