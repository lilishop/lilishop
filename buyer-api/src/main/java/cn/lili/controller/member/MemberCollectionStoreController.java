package cn.lili.controller.member;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
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
@Api(tags = "买家端,会员店铺收藏接口")
@RequestMapping("/buyer/member/storeCollection")
public class MemberCollectionStoreController {

    /**
     * 会员店铺
     */
    @Autowired
    private StoreCollectionService storeCollectionService;

    @ApiOperation(value = "查询会员收藏列表")
    @GetMapping("/STORE")
    public ResultMessage<Object> goodsList(PageVO page) {
        return ResultUtil.data(storeCollectionService.storeCollection(page));
    }

    @ApiOperation(value = "添加会员收藏")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "num", value = "值", dataType = "Long", paramType = "path")
    })
    @PostMapping("/add/STORE/{id}")
    public ResultMessage<Object> addGoodsCollection(@NotNull(message = "值不能为空") @PathVariable String id) {
        return ResultUtil.data(storeCollectionService.addStoreCollection(id));

    }

    @ApiOperation(value = "删除会员收藏")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "num", value = "值", dataType = "Long", paramType = "path")
    })
    @DeleteMapping(value = "/delete/STORE/{id}")
    public ResultMessage<Object> deleteGoodsCollection(@NotNull(message = "值不能为空") @PathVariable String id) {
        return ResultUtil.data(storeCollectionService.deleteStoreCollection(id));
    }

    @ApiOperation(value = "查询会员是否收藏")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "值", dataType = "String", paramType = "path")
    })
    @GetMapping(value = "/isCollection/STORE/{id}")
    public ResultMessage<Boolean> isCollection(@NotNull(message = "值不能为空") @PathVariable String id) {
        return ResultUtil.data(this.storeCollectionService.isCollection(id));
    }
}
