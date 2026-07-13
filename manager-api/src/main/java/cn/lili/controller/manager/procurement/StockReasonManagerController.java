package cn.lili.controller.manager.procurement;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.procurement.entity.dos.StockReason;
import cn.lili.modules.procurement.service.StockReasonService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

/**
 * 管理端,出入库原因接口
 * 提供按条件分页查询原因
 *
 * @author Bulbasaur
 * @since 2025-12-18
 */
@Tag(name = "管理端,出入库原因接口")
@RestController
@RequestMapping("/manager/procurement/reason")
public class StockReasonManagerController {

    @Autowired
    private StockReasonService stockReasonService;

    @Operation(summary = "出入库原因列表")
    @GetMapping
    public ResultMessage<IPage<StockReason>> page(@RequestParam(required = false) String reason,
                                                 @RequestParam(required = false) String category,
                                                 PageVO pageVO) {
        return ResultUtil.data(stockReasonService.page(reason, category, pageVO));
    }

    @Operation(summary = "添加出入库原因")
    @PostMapping
    public ResultMessage<StockReason> add(@RequestBody StockReason stockReason) {
        stockReasonService.save(stockReason);
        return ResultUtil.data(stockReason);
    }

    @Operation(summary = "编辑出入库原因")
    @PutMapping
    public ResultMessage<Boolean> update(@RequestBody StockReason stockReason) {
        return ResultUtil.data(stockReasonService.updateById(stockReason));
    }

    @Operation(summary = "删除出入库原因")
    @Parameter(name = "id", description = "原因ID", required = true)
    @DeleteMapping("/{id}")
    public ResultMessage<Boolean> delete(@PathVariable String id) {
        return ResultUtil.data(stockReasonService.removeById(id));
    }
}
