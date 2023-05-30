package cn.lili.controller.goods;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.dos.Parameters;
import cn.lili.modules.goods.service.ParametersService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

/**
 * 管理端,分类绑定参数组管理接口
 *
 * @author Bulbasaur
 * @since 2020/11/26 16:15
 */
@RestController
@Api(tags = "管理端,分类绑定参数组管理接口")
@RequestMapping("/manager/goods/parameters")
public class ParameterManagerController {

    @Autowired
    private ParametersService parametersService;


    @ApiOperation(value = "添加参数")
    @PostMapping
    public ResultMessage<Parameters> save(@Valid Parameters parameters) {

        if (parametersService.save(parameters)) {
            return ResultUtil.data(parameters);
        }
        throw new ServiceException(ResultCode.PARAMETER_SAVE_ERROR);

    }

    @ApiOperation(value = "编辑参数")
    @PutMapping
    public ResultMessage<Parameters> update(@Valid Parameters parameters) {

        if (parametersService.updateParameter(parameters)) {
            return ResultUtil.data(parameters);
        }
        throw new ServiceException(ResultCode.PARAMETER_UPDATE_ERROR);
    }

    @ApiOperation(value = "通过id删除参数")
    @ApiImplicitParam(name = "id", value = "参数ID", required = true, paramType = "path")
    @DeleteMapping(value = "/{id}")
    public ResultMessage<Object> delById(@PathVariable String id) {
        parametersService.removeById(id);
        return ResultUtil.success();

    }

}
