package cn.lili.controller.member;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.service.GoodsCollectionService;
import cn.lili.modules.member.service.StoreCollectionService;
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
@Api(tags = "买家端,会员收藏接口")
@RequestMapping("/buyer/member/collection")
public class MemberCollectionController {

    /**
     * 会员商品收藏
     */
    @Autowired
    private GoodsCollectionService goodsCollectionService;
    /**
     * 会员店铺
     */
    @Autowired
    private StoreCollectionService storeCollectionService;

    /**
     * 商品收藏关键字
     */
    private String goods="GOODS";

    @ApiOperation(value = "查询会员收藏列表")
    @ApiImplicitParam(name = "type", value = "类型", dataType = "String", paramType = "path", example = "GOODS:商品,STORE:店铺")
    @GetMapping("/{type}")
    public ResultMessage<Object> goodsList(@PathVariable String type, PageVO page) {
        if (goods.equals(type)) {
            return ResultUtil.data(goodsCollectionService.goodsCollection(page));
        }
        return ResultUtil.data(storeCollectionService.storeCollection(page));
    }

    @ApiOperation(value = "添加会员收藏")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "type", value = "类型", dataType = "String", paramType = "path", example = "GOODS:商品,STORE:店铺"),
            @ApiImplicitParam(name = "num", value = "值", dataType = "Long", paramType = "path")
    })
    @PostMapping("/add/{type}/{id}")
    public ResultMessage<Object> addGoodsCollection(@PathVariable String type,
                                                    @NotNull(message = "值不能为空") @PathVariable String id) {
        if (goods.equals(type)) {
            return ResultUtil.data(goodsCollectionService.addGoodsCollection(id));
        }
        return ResultUtil.data(storeCollectionService.addStoreCollection(id));

    }

    @ApiOperation(value = "删除会员收藏")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "type", value = "类型", dataType = "String", paramType = "path", example = "GOODS:商品,STORE:店铺"),
            @ApiImplicitParam(name = "num", value = "值", dataType = "Long", paramType = "path")
    })
    @DeleteMapping(value = "/delete/{type}/{id}")
    public ResultMessage<Object> deleteGoodsCollection(@PathVariable String type,
                                                       @NotNull(message = "值不能为空") @PathVariable String id) {
        if (goods.equals(type)) {
            return ResultUtil.data(goodsCollectionService.deleteGoodsCollection(id));
        }
        return ResultUtil.data(storeCollectionService.deleteStoreCollection(id));
    }

    @ApiOperation(value = "查询会员是否收藏")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "type", value = "类型", dataType = "String", paramType = "path", example = "GOODS:商品,STORE:店铺"),
            @ApiImplicitParam(name = "id", value = "值", dataType = "String", paramType = "path")
    })
    @GetMapping(value = "/isCollection/{type}/{id}")
    public ResultMessage<Boolean> isCollection(@PathVariable String type,
                                               @NotNull(message = "值不能为空") @PathVariable String id) {
        if (goods.equals(type)) {
            return ResultUtil.data(this.goodsCollectionService.isCollection(id));
        }
        return ResultUtil.data(this.storeCollectionService.isCollection(id));
    }
}
