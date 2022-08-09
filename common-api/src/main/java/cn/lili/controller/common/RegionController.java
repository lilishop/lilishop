package cn.lili.controller.common;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.system.entity.dos.Region;
import cn.lili.modules.system.entity.vo.RegionVO;
import cn.lili.modules.system.service.RegionService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;


/**
 * 地址信息接口
 *
 * @author Chopper
 */
@RestController
@Api(tags = "地址信息接口")
@RequestMapping("/common/common/region")
public class RegionController {

    @Autowired
    private RegionService regionService;

    @ApiOperation(value = "点地图获取地址信息")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "cityCode", value = "城市code", dataType = "String", paramType = "query"),
            @ApiImplicitParam(name = "townName", value = "镇名称", dataType = "Long", paramType = "query")
    })
    @GetMapping(value = "/region")
    public ResultMessage<Object> getRegion(@RequestParam String cityCode,@RequestParam String townName) {
        return ResultUtil.data(regionService.getRegion(cityCode,townName));
    }

    @GetMapping(value = "/name")
    @ApiOperation(value = "根据名字获取地区地址id")
    public ResultMessage<String> getItemByLastName(String lastName) {
        return ResultUtil.data(regionService.getItemByLastName(lastName));
    }

    @GetMapping(value = "/item/{id}")
    @ApiImplicitParam(name = "id", value = "地区ID", required = true, dataType = "String", paramType = "path")
    @ApiOperation(value = "通过id获取子地区")
    public ResultMessage<List<Region>> getItem(@PathVariable String id) {
        return ResultUtil.data(regionService.getItem(id));
    }

    @GetMapping(value = "/allCity")
    @ApiOperation(value = "获取所有的省-市")
    public ResultMessage<List<RegionVO>> getAllCity() {
        return ResultUtil.data(regionService.getAllCity());
    }


}
