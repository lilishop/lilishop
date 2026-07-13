package cn.lili.controller.manager.order;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.order.aftersale.entity.dos.AfterSaleReason;
import cn.lili.modules.order.aftersale.service.AfterSaleReasonService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;

/**
 * 管理端,售后原因接口
 *
 * @author Bulbasaur
 * @since 2021/1/6 14:11
 */
@RestController
@RequestMapping("/manager/order/afterSaleReason")
@Tag(name = "管理端,售后原因接口")
public class AfterSaleReasonManagerController {

    /**
     * 售后原因
     */
    @Autowired
    private AfterSaleReasonService afterSaleReasonService;

    @Operation(description = "查看售后原因")
    @Parameter(name = "id", description = "售后原因ID", required = true)
    @GetMapping("/{id}")
    public ResultMessage<AfterSaleReason> get(@PathVariable String id) {

        return ResultUtil.data(afterSaleReasonService.getById(id));
    }

    @Operation(description = "分页获取售后原因")
    @Parameter(name = "serviceType", description = "售后类型", required = true)
    @GetMapping("/getByPage")
    public ResultMessage<IPage<AfterSaleReason>> getByPage(PageVO page, @RequestParam String serviceType) {
        return ResultUtil.data(afterSaleReasonService.pageByServiceType(page, serviceType));
    }

    @Operation(description = "添加售后原因")
    @Parameter(name = "afterSaleReason", description = "售后原因", required = true)
    @PostMapping
    public ResultMessage<AfterSaleReason> save(@Valid AfterSaleReason afterSaleReason) {
        afterSaleReasonService.save(afterSaleReason);
        return ResultUtil.data(afterSaleReason);
    }

    @Operation(description = "修改售后原因")
    @Parameter(name = "id", description = "售后原因ID", required = true)
    @PutMapping("update/{id}")
    public ResultMessage<AfterSaleReason> update(@Valid AfterSaleReason afterSaleReason, @PathVariable("id") String id) {
        afterSaleReason.setId(id);
        return ResultUtil.data(afterSaleReasonService.editAfterSaleReason(afterSaleReason));
    }

    @Operation(description = "删除售后原因")
    @Parameter(name = "id", description = "售后原因ID", required = true)
    @DeleteMapping("/delByIds/{id}")
    public ResultMessage<Object> delAllByIds(@PathVariable String id) {
        afterSaleReasonService.removeById(id);
        return ResultUtil.success();
    }
}
