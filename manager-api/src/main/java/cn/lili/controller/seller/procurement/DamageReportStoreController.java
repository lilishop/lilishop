package cn.lili.controller.seller.procurement;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.procurement.entity.dos.DamageReport;
import cn.lili.modules.procurement.entity.dos.DamageReportItem;
import cn.lili.modules.procurement.entity.dto.DamageReportCreateDTO;
import cn.lili.modules.procurement.service.DamageReportItemService;
import cn.lili.modules.procurement.service.DamageReportService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.web.bind.annotation.*;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * 店铺端报损单接口
 * 提供报损单的创建、提交、审核、作废、完成等操作
 * @author Bulbasaur
 * @since 2025-12-18
 */
@RestController
@Tag(name = "店铺端,报损单接口")
@RequestMapping("/store/procurement/damage-report")
public class DamageReportStoreController {

    @Autowired
    private DamageReportService damageReportService;
    @Autowired
    private DamageReportItemService damageReportItemService;

    @Operation(summary = "新建报损单")
    @PostMapping
    public ResultMessage<DamageReport> create(@RequestBody DamageReportCreateDTO dto) {
        return ResultUtil.data(damageReportService.create(dto));
    }

    @Operation(summary = "提交报损单")
    @Parameter(name = "id", description = "报损单ID", required = true)
    @PutMapping("/{id}/submit")
    public ResultMessage<DamageReport> submit(@PathVariable String id) {
        return ResultUtil.data(damageReportService.submit(id));
    }

    @Operation(summary = "审核通过")
    @Parameter(name = "id", description = "报损单ID", required = true)
    @PutMapping("/{id}/approve")
    public ResultMessage<DamageReport> approve(@PathVariable String id) {
        return ResultUtil.data(damageReportService.approve(id));
    }

    @Operation(summary = "审核驳回")
    @Parameter(name = "id", description = "报损单ID", required = true)
    @Parameter(name = "remark", description = "驳回备注", required = true)
    @PutMapping("/{id}/reject")
    public ResultMessage<DamageReport> reject(@PathVariable String id, @RequestParam String remark) {
        return ResultUtil.data(damageReportService.reject(id, remark));
    }

    @Operation(summary = "作废报损单")
    @Parameter(name = "id", description = "报损单ID", required = true)
    @PutMapping("/{id}/cancel")
    public ResultMessage<DamageReport> cancel(@PathVariable String id) {
        return ResultUtil.data(damageReportService.cancel(id));
    }

    @Operation(summary = "完成报损并扣减库存")
    @Parameter(name = "id", description = "报损单ID", required = true)
    @PutMapping("/{id}/complete")
    public ResultMessage<DamageReport> complete(@PathVariable String id) {
        return ResultUtil.data(damageReportService.complete(id));
    }

    @Operation(summary = "获取报损单详情")
    @Parameter(name = "id", description = "报损单ID", required = true)
    @GetMapping("/{id}")
    public ResultMessage<DamageReport> get(@PathVariable String id) {
        return ResultUtil.data(OperationalJudgment.judgment(damageReportService.getById(id)));
    }

    @Operation(summary = "获取报损单明细")
    @Parameter(name = "id", description = "报损单ID", required = true)
    @GetMapping("/{id}/items")
    public ResultMessage<List<DamageReportItem>> items(@PathVariable String id) {
        DamageReport report = OperationalJudgment.judgment(damageReportService.getById(id));
        return ResultUtil.data(damageReportItemService.listByReportId(report.getId()));
    }

    @Operation(summary = "分页查询报损单")
    @GetMapping("/page")
    public ResultMessage<IPage<DamageReport>> page(PageVO page,
                                                   @RequestParam(required = false) String status,
                                                   @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss") @RequestParam(required = false) Date startDate,
                                                   @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss") @RequestParam(required = false) Date endDate) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        return ResultUtil.data(damageReportService.pageByCondition(page, storeId, status, startDate, endDate));
    }

    @Operation(summary = "报损单各状态数量")
    @GetMapping("/status-count")
    public ResultMessage<Map<String, Long>> statusCount(
            @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss") @RequestParam(required = false) Date startDate,
            @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss") @RequestParam(required = false) Date endDate) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        return ResultUtil.data(damageReportService.statusCount(storeId, startDate, endDate));
    }
}
