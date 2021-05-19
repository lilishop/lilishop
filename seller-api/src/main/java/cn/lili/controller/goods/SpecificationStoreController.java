package cn.lili.controller.goods;


import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.dos.Specification;
import cn.lili.modules.goods.entity.dto.SpecificationSearchParams;
import cn.lili.modules.goods.entity.vos.SpecificationVO;
import cn.lili.modules.goods.service.SpecificationService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;


/**
 * 店铺端,规格管理接口
 *
 * @author pikachu
 * @date 2020-02-18 15:18:56
 */
@RestController
@Api(tags = "店铺端,规格管理接口")
@RequestMapping("/store/goods/spec")
public class SpecificationStoreController {
    @Autowired
    private SpecificationService specificationService;


    @GetMapping(value = "/page")
    @ApiOperation(value = "分页获取")
    public ResultMessage<IPage<Specification>> getByPage(SpecificationSearchParams searchParams, PageVO pageVo) {
        searchParams.setDeleteFlag(false);
        return ResultUtil.data(specificationService.getSpecificationByPage(searchParams, pageVo));
    }

    @PostMapping
    @ApiOperation(value = "添加规格")
    public ResultMessage<Specification> save(@Valid SpecificationVO parameters) {
        if (parameters.getStoreId() == null) {
            parameters.setStoreId(UserContext.getCurrentUser().getId());
        }
        specificationService.addSpecification(parameters);
        return ResultUtil.data(parameters);
    }

    @DeleteMapping(value = "/{ids}")
    @ApiImplicitParam(name = "ids", value = "规格ID", required = true, dataType = "String", allowMultiple = true, paramType = "path")
    @ApiOperation(value = "批量删除")
    public ResultMessage<Object> delAllByIds(@PathVariable List<String> ids) {
        specificationService.deleteSpecification(ids);
        return ResultUtil.success();
    }
}
