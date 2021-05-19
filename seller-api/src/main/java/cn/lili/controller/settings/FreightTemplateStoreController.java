package cn.lili.controller.settings;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.store.entity.vos.FreightTemplateVO;
import cn.lili.modules.store.service.FreightTemplateService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

/**
 * 店铺端,运费模板接口
 *
 * @author paulG
 * @date 2020/8/26
 **/
@RestController
@Api(tags = "店铺端,运费模板接口")
@RequestMapping("/store/freightTemplate")
public class FreightTemplateStoreController {
    @Autowired
    private FreightTemplateService freightTemplateService;

    @ApiOperation(value = "商家运费模板列表")
    @GetMapping
    private ResultMessage<List<FreightTemplateVO>> list() {
        return ResultUtil.data(freightTemplateService.getFreightTemplateList(UserContext.getCurrentUser().getStoreId()));
    }

    @ApiOperation(value = "获取商家运费模板详情")
    @ApiImplicitParam(name = "id", value = "商家模板ID", required = true, paramType = "path")
    @GetMapping("/{id}")
    private ResultMessage<FreightTemplateVO> list(@PathVariable String id) {
        return ResultUtil.data(freightTemplateService.getFreightTemplate(id));
    }

    @ApiOperation(value = "添加商家运费模板")
    @PostMapping
    public ResultMessage<FreightTemplateVO> add(@Valid @RequestBody FreightTemplateVO freightTemplateVO) {
        return ResultUtil.data(freightTemplateService.addFreightTemplate(freightTemplateVO));
    }

    @ApiOperation(value = "修改商家运费模板")
    @PutMapping("/{id}")
    public ResultMessage<FreightTemplateVO> edit(@PathVariable String id, @RequestBody @Valid FreightTemplateVO freightTemplateVO) {
        return ResultUtil.data(freightTemplateService.editFreightTemplate(freightTemplateVO));
    }

    @ApiOperation(value = "删除商家运费模板")
    @ApiImplicitParam(name = "id", value = "商家模板ID", required = true, paramType = "path")
    @DeleteMapping("/{id}")
    public ResultMessage<Object> edit(@PathVariable String id) {
        freightTemplateService.removeFreightTemplate(id);
        return ResultUtil.success();
    }
}
