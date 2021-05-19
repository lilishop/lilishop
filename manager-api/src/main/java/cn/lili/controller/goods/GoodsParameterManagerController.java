package cn.lili.controller.goods;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.vos.GoodsParamsGroupVO;
import cn.lili.modules.goods.service.GoodsParamsService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * 管理端,商品关联参数管理接口
 *
 * @author pikachu
 * @date 2020-02-18 15:18:56
 */
@RestController
@Api(tags = "管理端,商品关联参数管理接口")
@RequestMapping("/manager/goods/parameters")
public class GoodsParameterManagerController {

    @Autowired
    private GoodsParamsService goodsParamsService;

    @ApiImplicitParams({
            @ApiImplicitParam(name = "goodsId", value = "商品ID", required = true, paramType = "path", dataType = "String"),
            @ApiImplicitParam(name = "categoryId", value = "分类ID", required = true, paramType = "path", dataType = "String")
    })
    @ApiOperation(value = "通过商品id和分类id查询参数信息")
    @GetMapping(value = "/{goodsId}/{categoryId}")
    public ResultMessage<List<GoodsParamsGroupVO>> getGoodsParameters(@PathVariable String goodsId, @PathVariable String categoryId) {
        return ResultUtil.data(this.goodsParamsService.queryGoodsParams(goodsId, categoryId));
    }

}
