package cn.lili.controller.seller.common;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.system.entity.dos.Region;
import cn.lili.modules.system.entity.vo.RegionVO;
import cn.lili.modules.system.service.RegionService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 地址信息接口
 *
 * @author Chopper
 */
@RestController
@Tag(name = "地址信息接口")
@RequestMapping("/store/common/region")
public class RegionStoreController {

    @Autowired
    private RegionService regionService;

    @Operation(summary = "点地图获取地址信息")
    @Parameters({
            @Parameter(name = "cityCode", description = "城市code"),
            @Parameter(name = "townName", description = "镇名称")
    })
    @GetMapping("/region")
    public ResultMessage<Object> getRegion(@RequestParam String cityCode, @RequestParam String townName) {
        return ResultUtil.data(regionService.getRegion(cityCode, townName));
    }

    @GetMapping("/name")
    @Operation(summary = "根据名字获取地区地址id")
    public ResultMessage<String> getItemByLastName(String lastName) {
        return ResultUtil.data(regionService.getItemByLastName(lastName));
    }

    @GetMapping("/item/{id}")
    @Parameter(name = "id", description = "地区ID", required = true)
    @Operation(summary = "通过id获取子地区")
    public ResultMessage<List<Region>> getItem(@PathVariable String id) {
        return ResultUtil.data(regionService.getItem(id));
    }

    @GetMapping("/allCity")
    @Operation(summary = "获取所有的省-市")
    public ResultMessage<List<RegionVO>> getAllCity() {
        return ResultUtil.data(regionService.getAllCity());
    }

}
