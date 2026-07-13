package cn.lili.controller.manager.goods;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.dos.Parameters;
import cn.lili.modules.goods.entity.dto.GoodsParamsDTO;
import cn.lili.modules.goods.entity.dto.GoodsParamsSearchDTO;
import cn.lili.modules.goods.service.CategoryParameterService;
import cn.lili.modules.goods.service.ParametersService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

/**
 * 管理端,商品参数管理接口
 *
 * @author Bulbasaur
 * @since 2020/11/26 16:15
 */
@RestController
@Tag(name = "管理端,商品参数管理接口")
@RequestMapping("/manager/goods/parameters")
public class ParameterManagerController {

    @Autowired
    private ParametersService parametersService;

    @Autowired
    private CategoryParameterService categoryParameterService;

    @Operation(summary = "商品参数分页列表")
    @Parameter(name = "pageVo", description = "分页参数")
    @GetMapping
    public ResultMessage<IPage<Parameters>> page(GoodsParamsSearchDTO goodsParamsSearchDTO) {
        return ResultUtil.data(parametersService.parametersPage(goodsParamsSearchDTO));
    }

    @Operation(summary = "添加商品参数")
    @PostMapping
    public ResultMessage<Parameters> save(@Valid GoodsParamsDTO parameters) {
        if (parametersService.addParameter(parameters)) {
            return ResultUtil.data(parameters);
        }
        throw new ServiceException(ResultCode.PARAMETER_SAVE_ERROR);

    }

    @Operation(summary = "编辑商品参数")
    @PutMapping
    public ResultMessage<Parameters> update(@Valid GoodsParamsDTO parameters) {
        if (parametersService.updateParameter(parameters)) {
            return ResultUtil.data(parameters);
        }
        throw new ServiceException(ResultCode.PARAMETER_UPDATE_ERROR);
    }

    @Operation(summary = "通过id删除商品参数")
    @Parameter(name = "id", description = "参数ID", required = true)
    @DeleteMapping("/{id}")
    public ResultMessage<Object> delById(@PathVariable String id) {
        categoryParameterService.deleteByParameterId(id);
        parametersService.removeById(id);
        return ResultUtil.success();

    }

    @Operation(summary = "商品参数详细")
    @GetMapping("/{id}")
    public ResultMessage<GoodsParamsDTO> page(@PathVariable String id) {
        return ResultUtil.data(parametersService.getGoodsParamsDTO(id));
    }

}
