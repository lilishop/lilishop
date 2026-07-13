package cn.lili.controller.seller.goods;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.dos.StoreGoodsLabel;
import cn.lili.modules.goods.entity.vos.StoreGoodsLabelVO;
import cn.lili.modules.goods.service.StoreGoodsLabelService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Objects;


/**
 * 店铺端,店铺分类接口
 *
 * @author Bulbasaur
 * @since 2020/11/17 2:32 下午
 */
@Tag(name = "店铺端,店铺分类接口")
@RestController
@RequestMapping("/store/goods/label")
public class GoodsLabelStoreController {

    /**
     * 店铺分类
     */
    @Autowired
    private StoreGoodsLabelService storeGoodsLabelService;

    @Operation(summary = "获取当前店铺商品分类列表")
    @GetMapping
    public ResultMessage<List<StoreGoodsLabelVO>> list() {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        return ResultUtil.data(storeGoodsLabelService.listByStoreId(storeId));
    }

    @Parameter(name = "id", description = "店铺商品分类ID", required = true)
    @Operation(summary = "获取店铺商品分类详情")
    @GetMapping("/get/{id}")
    public ResultMessage<StoreGoodsLabel> getStoreGoodsLabel(@PathVariable String id) {
        return ResultUtil.data(OperationalJudgment.judgment(storeGoodsLabelService.getById(id)));
    }

    @Operation(summary = "添加店铺商品分类")
    @PostMapping
    public ResultMessage<StoreGoodsLabel> add(@Validated StoreGoodsLabel storeGoodsLabel) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        storeGoodsLabel.setStoreId(storeId);
        return ResultUtil.data(storeGoodsLabelService.addStoreGoodsLabel(storeGoodsLabel));
    }

    @Operation(summary = "修改店铺商品分类")
    @PutMapping
    public ResultMessage<StoreGoodsLabel> edit(@Validated StoreGoodsLabel storeGoodsLabel) {
        OperationalJudgment.judgment(storeGoodsLabelService.getById(storeGoodsLabel.getId()));
        return ResultUtil.data(storeGoodsLabelService.editStoreGoodsLabel(storeGoodsLabel));
    }

    @Parameter(name = "id", description = "店铺商品分类ID", required = true)
    @Operation(summary = "删除店铺商品分类")
    @DeleteMapping("/{id}")
    public ResultMessage<StoreGoodsLabel> delete(@PathVariable String id) {
        OperationalJudgment.judgment(storeGoodsLabelService.getById(id));
        storeGoodsLabelService.removeStoreGoodsLabel(id);
        return ResultUtil.success();
    }
}
