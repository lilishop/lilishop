package cn.lili.controller.goods;


import cn.lili.common.utils.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.dos.SpecValues;
import cn.lili.modules.goods.service.SpecValuesService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.constraints.NotNull;
import java.util.List;

/**
 * 店铺端,规格项管理接口
 *
 * @author pikachu
 * @date 2020-02-18 15:18:56
 */
@RestController
@Api(tags = "店铺端,规格项管理接口")
@RequestMapping("/store/goods/spec-values")
public class SpecValuesStoreController {
    @Autowired
    private SpecValuesService specValuesService;

    @GetMapping(value = "/values/{id}")
    @ApiImplicitParam(name = "id", value = "规格项ID", required = true, dataType = "String", paramType = "path")
    @ApiOperation(value = "查询规格值列表")
    public ResultMessage<IPage<SpecValues>> list(@PathVariable("id") String id, String specVal, PageVO pageVo) {
        return ResultUtil.data(specValuesService.queryByParams(id, specVal, pageVo));
    }

    @ApiOperation(value = "保存规格值")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "商品规格ID", required = true, paramType = "path"),
            @ApiImplicitParam(name = "specValue", value = "商品项", required = true, allowMultiple = true, paramType = "query")
    })
    @PostMapping(value = "/save/{id}")
    public ResultMessage<List<SpecValues>> saveSpecValue(@PathVariable("id") String specId,
                                                         @NotNull(message = "至少添加一个规格值") @RequestParam(value = "spec_value") String[] specValue) {
        //重新添加
        List<SpecValues> list = specValuesService.addSpecValue(specId, specValue);
        return ResultUtil.data(list);
    }


}
