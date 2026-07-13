package cn.lili.controller.manager.promotion;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.promotion.entity.dos.PointsGoodsCategory;
import cn.lili.modules.promotion.entity.vos.PointsGoodsCategoryVO;
import cn.lili.modules.promotion.service.PointsGoodsCategoryService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

/**
 * 管理端,积分商品分类接口
 *
 * @author paulG
 * @since 2021/1/14
 **/
@RestController
@Tag(name = "管理端,积分商品分类接口")
@RequestMapping("/manager/promotion/pointsGoodsCategory")
public class PointsGoodsCategoryManagerController {
    @Autowired
    private PointsGoodsCategoryService pointsGoodsCategoryService;

    @Operation(summary = "添加积分商品分类")
    @Parameter(name = "pointsGoodsCategory", description = "积分商品分类VO", required = true)
    @PostMapping
    public ResultMessage<Object> add(PointsGoodsCategoryVO pointsGoodsCategory) {
        pointsGoodsCategoryService.addCategory(pointsGoodsCategory);
        return ResultUtil.success();
    }

    @Operation(summary = "修改积分商品分类")
    @Parameter(name = "pointsGoodsCategory", description = "积分商品分类VO", required = true)
    @PutMapping
    public ResultMessage<Object> update(PointsGoodsCategoryVO pointsGoodsCategory) {
        pointsGoodsCategoryService.updateCategory(pointsGoodsCategory);
        return ResultUtil.success();
    }

    @Operation(summary = "删除积分商品分类")
    @Parameter(name = "id", description = "积分商品分类ID", required = true)
    @DeleteMapping("/{id}")
    public ResultMessage<Object> delete(@PathVariable String id) {
        pointsGoodsCategoryService.deleteCategory(id);
        return ResultUtil.success();
    }

    @Operation(summary = "获取积分商品分类分页")
    @Parameter(name = "name", description = "积分商品分类名称")
    @Parameter(name = "page", description = "分页参数")
    @GetMapping
    public ResultMessage<IPage<PointsGoodsCategory>> page(String name, PageVO page) {
        return ResultUtil.data(pointsGoodsCategoryService.getCategoryByPage(name, page));
    }
    
    @Operation(summary = "获取积分商品分类详情")
    @Parameter(name = "id", description = "积分商品分类ID", required = true)
    @GetMapping("/{id}")
    public ResultMessage<Object> getById(@PathVariable String id) {
        return ResultUtil.data(pointsGoodsCategoryService.getCategoryDetail(id));
    }

}
