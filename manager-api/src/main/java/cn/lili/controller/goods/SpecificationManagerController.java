package cn.lili.controller.goods;


import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.dos.Specification;
import cn.lili.modules.goods.service.SpecificationService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;


/**
 * 管理端,商品规格接口
 *
 * @author pikachu
 * @since 2020-02-18 15:18:56
 */
@RestController
@Api(tags = "管理端,商品规格接口")
@RequestMapping("/manager/goods/spec")
public class SpecificationManagerController {

    @Autowired
    private SpecificationService specificationService;


    @GetMapping("/all")
    @ApiOperation(value = "获取所有可用规格")
    public ResultMessage<List<Specification>> getAll() {
        return ResultUtil.data(specificationService.list());
    }

    @GetMapping
    @ApiOperation(value = "搜索规格")
    public ResultMessage<Page<Specification>> page(String specName, PageVO page) {
        LambdaQueryWrapper<Specification> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper.like(CharSequenceUtil.isNotEmpty(specName), Specification::getSpecName, specName);
        return ResultUtil.data(specificationService.page(PageUtil.initPage(page), lambdaQueryWrapper));
    }

    @PostMapping
    @ApiOperation(value = "保存规格")
    public ResultMessage<Object> save(@Valid Specification specification) {
        specificationService.save(specification);
        return ResultUtil.success();
    }

    @PutMapping("/{id}")
    @ApiOperation(value = "更改规格")
    public ResultMessage<Object> update(@Valid Specification specification, @PathVariable String id) {
        specification.setId(id);
        return ResultUtil.data(specificationService.saveOrUpdate(specification));
    }

    @DeleteMapping("/{ids}")
    @ApiImplicitParam(name = "ids", value = "规格ID", required = true, dataType = "String", allowMultiple = true, paramType = "path")
    @ApiOperation(value = "批量删除")
    public ResultMessage<Object> delAllByIds(@PathVariable List<String> ids) {
        return ResultUtil.data(specificationService.deleteSpecification(ids));
    }
}
