package cn.lili.controller.manager.setting;

import cn.lili.common.aop.annotation.DemoSite;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.system.entity.dos.Region;
import cn.lili.modules.system.service.RegionService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.util.List;

/**
 * 管理端,行政地区管理接口
 *
 * @author Chopper
 * @since 2020/12/2 10:40
 */
@RestController
@Tag(name = "管理端,行政地区管理接口")
@RequestMapping("/manager/setting/region")
public class RegionSettingManagerController {
    @Autowired
    private RegionService regionService;

    @DemoSite
    @PostMapping("/sync")
    @Operation(summary = "同步高德行政地区数据")
    @Parameter(name = "url", description = "高德行政地区数据接口", required = true)
    public void synchronizationData(String url) {
        regionService.synchronizationData(url);
    }

    @GetMapping("/{id}")
    @Operation(summary = "通过id获取地区详情")
    @Parameter(name = "id", description = "地区ID", required = true)
    public ResultMessage<Region> get(@PathVariable String id) {
        return ResultUtil.data(regionService.getById(id));
    }

    @GetMapping("/item/{id}")
    @Operation(summary = "通过id获取子地区")
    @Parameter(name = "id", description = "地区ID", required = true)
    public ResultMessage<List<Region>> getItem(@PathVariable String id) {
        return ResultUtil.data(regionService.getItem(id));
    }

    @DemoSite
    @PutMapping("/{id}")
    @Operation(summary = "更新地区")
    @Parameter(name = "id", description = "地区ID", required = true)
    @Parameter(name = "region", description = "地区信息", required = true)
    public ResultMessage<Region> update(@PathVariable String id, @Valid Region region) {
        region.setId(id);
        regionService.updateById(region);
        return ResultUtil.data(region);
    }

    @DemoSite
    @PostMapping("/save")
    @Operation(summary = "增加地区")
    @Parameter(name = "region", description = "地区信息", required = true)
    public ResultMessage<Region> save(@Valid Region region) {
        regionService.save(region);
        return ResultUtil.data(region);
    }

    @DemoSite
    @DeleteMapping("/{ids}")
    @Operation(summary = "批量通过id删除")
    @Parameter(name = "ids", description = "地区ID列表", required = true)
    public ResultMessage<Object> delAllByIds(@PathVariable List<String> ids) {
        regionService.removeByIds(ids);
        return ResultUtil.success();
    }
}
