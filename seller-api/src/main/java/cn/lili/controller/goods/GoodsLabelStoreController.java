package cn.lili.controller.goods;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.store.entity.dos.StoreGoodsLabel;
import cn.lili.modules.store.entity.vos.StoreGoodsLabelVO;
import cn.lili.modules.store.service.StoreGoodsLabelService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;


/**
 * 店铺端,店铺分类接口
 *
 * @author Bulbasaur
 * @date 2020/11/17 2:32 下午
 */
@Api(tags = "店铺端,店铺分类接口")
@RestController
@RequestMapping("/store/goods/label")
public class GoodsLabelStoreController {

    /**
     * 店铺分类
     */
    @Autowired
    private StoreGoodsLabelService storeGoodsLabelService;

    @ApiOperation(value = "获取当前店铺商品分类列表")
    @GetMapping
    public ResultMessage<List<StoreGoodsLabelVO>> list() {
        return ResultUtil.data(storeGoodsLabelService.listByStoreId(UserContext.getCurrentUser().getStoreId()));
    }

    @ApiImplicitParam(name = "id", value = "店铺商品分类ID", required = true, paramType = "path")
    @ApiOperation(value = "获取店铺商品分类详情")
    @GetMapping("/get/{id}")
    public ResultMessage<StoreGoodsLabel> getStoreGoodsLabel(@PathVariable String id) {
        return ResultUtil.data(storeGoodsLabelService.getById(id));
    }

    @ApiOperation(value = "添加店铺商品分类")
    @PostMapping
    public ResultMessage<StoreGoodsLabel> add(StoreGoodsLabel storeGoodsLabel) {
        return ResultUtil.data(storeGoodsLabelService.addStoreGoodsLabel(storeGoodsLabel));
    }

    @ApiOperation(value = "修改店铺商品分类")
    @PutMapping
    public ResultMessage<StoreGoodsLabel> edit(StoreGoodsLabel storeGoodsLabel) {
        return ResultUtil.data(storeGoodsLabelService.editStoreGoodsLabel(storeGoodsLabel));
    }

    @ApiImplicitParam(name = "id", value = "店铺商品分类ID", required = true, paramType = "path")
    @ApiOperation(value = "删除店铺商品分类")
    @DeleteMapping("/{id}")
    public ResultMessage<StoreGoodsLabel> delete(@PathVariable String id) {
        storeGoodsLabelService.removeStoreGoodsLabel(id);
        return ResultUtil.success();
    }
}
