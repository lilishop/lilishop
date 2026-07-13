package cn.lili.controller.manager.other;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.system.entity.dos.Logistics;
import cn.lili.modules.system.service.LogisticsService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotNull;

/**
 * 管理端,物流公司接口
 *
 * @author Chopper
 * @since 2020/11/17 7:56 下午
 */
@RestController
@Tag(name = "管理端,物流公司接口")
@RequestMapping("/manager/other/logistics")
public class LogisticsManagerController {
    @Autowired
    private LogisticsService logisticsService;

    @Operation(summary = "通过id获取物流公司")
    @GetMapping("/{id}")
    public ResultMessage<Logistics> get(
            @Parameter(description = "物流公司ID", required = true) @PathVariable String id) {
        return ResultUtil.data(logisticsService.getById(id));
    }

    @Operation(summary = "分页获取物流公司")
    @GetMapping(value = "/getByPage")
    public ResultMessage<IPage<Logistics>> getByPage(PageVO page) {
        return ResultUtil.data(logisticsService.page(PageUtil.initPage(page)));
    }

    @Operation(summary = "编辑物流公司")
    @PutMapping("/{id}")
    public ResultMessage<Logistics> update(
            @Parameter(description = "物流公司ID", required = true) @NotNull @PathVariable String id, @Valid Logistics logistics) {
        logistics.setId(id);
        logisticsService.updateById(logistics);
        return ResultUtil.data(logistics);
    }

    @Operation(summary = "添加物流公司")
    @PostMapping
    public ResultMessage<Logistics> save(@Valid Logistics logistics) {
        logisticsService.save(logistics);
        return ResultUtil.data(logistics);
    }

    @Operation(summary = "删除物流公司")
    @DeleteMapping("/{id}")
    public ResultMessage<Object> delAllByIds(
            @Parameter(description = "物流公司ID", required = true) @PathVariable String id) {
        logisticsService.removeById(id);
        return ResultUtil.success();
    }
}
