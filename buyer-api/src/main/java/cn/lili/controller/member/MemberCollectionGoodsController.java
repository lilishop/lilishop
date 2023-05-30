package cn.lili.controller.member;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.service.GoodsCollectionService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.constraints.NotNull;

/**
 * 买家端,会员收藏接口
 *
 * @author Chopper
 * @since 2020/11/17 2:32 下午
 */
@RestController
@Api(tags = "买家端,会员商品收藏接口")
@RequestMapping("/buyer/member/collection")
public class MemberCollectionGoodsController {

    /**
     * 会员商品收藏
     */
    @Autowired
    private GoodsCollectionService goodsCollectionService;

    @ApiOperation(value = "查询会员收藏列表")
    @GetMapping("/GOODS")
    public ResultMessage<Object> goodsList(PageVO page) {
        return ResultUtil.data(goodsCollectionService.goodsCollection(page));
    }

    @ApiOperation(value = "添加会员收藏")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "num", value = "值", dataType = "Long", paramType = "path")
    })
    @PostMapping("/add/GOODS/{id}")
    public ResultMessage<Object> addGoodsCollection(@NotNull(message = "值不能为空") @PathVariable String id) {
        return ResultUtil.data(goodsCollectionService.addGoodsCollection(id));

    }

    @ApiOperation(value = "删除会员收藏")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "type", value = "类型", dataType = "String", paramType = "path", example = "GOODS:商品,STORE:店铺"),
            @ApiImplicitParam(name = "num", value = "值", dataType = "Long", paramType = "path")
    })
    @DeleteMapping(value = "/delete/GOODS/{id}")
    public ResultMessage<Object> deleteGoodsCollection(@NotNull(message = "值不能为空") @PathVariable String id) {
        return ResultUtil.data(goodsCollectionService.deleteGoodsCollection(id));
    }

    @ApiOperation(value = "查询会员是否收藏")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "type", value = "类型", dataType = "String", paramType = "path", example = "GOODS:商品,STORE:店铺"),
            @ApiImplicitParam(name = "id", value = "值", dataType = "String", paramType = "path")
    })
    @GetMapping(value = "/isCollection/GOODS/{id}")
    public ResultMessage<Boolean> isCollection(@NotNull(message = "值不能为空") @PathVariable String id) {
        return ResultUtil.data(this.goodsCollectionService.isCollection(id));
    }
}