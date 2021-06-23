package cn.lili.controller.goods;


import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.dos.Specification;
import cn.lili.modules.goods.entity.dto.SpecificationSearchParams;
import cn.lili.modules.goods.entity.vos.SpecificationVO;
import cn.lili.modules.goods.service.SpecificationService;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
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
 * @date 2020-02-18 15:18:56
 */
@RestController
@Api(tags = "管理端,商品规格接口")
@RequestMapping("/manager/goods/spec")
public class SpecificationManagerController {

    @Autowired
    private SpecificationService specificationService;

    @GetMapping(value = "/{id}")
    @ApiImplicitParam(name = "id", value = "商品规格ID", required = true, dataType = "String", paramType = "path")
    @ApiOperation(value = "通过id获取商品规格")
    public ResultMessage<Specification> get(@PathVariable String id) {
        return ResultUtil.data(specificationService.getSpecification(id));
    }

    @RequestMapping(value = "/all", method = RequestMethod.GET)
    @ApiOperation(value = "获取所有可用规格")
    public List<Specification> getAll() {
        List<Specification> list = specificationService.list(new QueryWrapper<Specification>().eq("delete_flag", 0));
        return list;
    }


    @GetMapping(value = "/page")
    @ApiOperation(value = "分页获取")
    public ResultMessage<IPage<SpecificationVO>> getByPage(String specId, PageVO pageVo) {
        SpecificationSearchParams searchParams = new SpecificationSearchParams();
        searchParams.setSpecId(specId);
        return ResultUtil.data(specificationService.getSpecificationPage(searchParams, pageVo));
    }

    @PutMapping
    @ApiOperation(value = "编辑规格")
    public ResultMessage<Specification> update(@Valid SpecificationVO parameters) {
        if (parameters.getStoreId() == null) {
            parameters.setStoreId("0");
        }
        if (specificationService.updateSpecification(parameters)) {
            return ResultUtil.data(parameters);
        }
        throw new ServiceException(ResultCode.SPEC_UPDATE_ERROR);
    }

    @PostMapping
    @ApiOperation(value = "添加规格")
    public ResultMessage<Specification> save(@Valid SpecificationVO parameters) {
        if (parameters.getStoreId() == null) {
            parameters.setStoreId("0");
        }
        if (specificationService.addSpecification(parameters) != null) {
            return ResultUtil.data(parameters);
        }
        throw new ServiceException(ResultCode.SPEC_SAVE_ERROR);
    }

    @DeleteMapping(value = "/{ids}")
    @ApiImplicitParam(name = "ids", value = "规格ID", required = true, dataType = "String", allowMultiple = true, paramType = "path")
    @ApiOperation(value = "批量删除")
    public ResultMessage<Object> delAllByIds(@PathVariable List<String> ids) {
        specificationService.deleteSpecification(ids);
        return ResultUtil.success();
    }
}
