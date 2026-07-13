package cn.lili.controller.manager.goods;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.dos.Category;
import cn.lili.modules.goods.service.CategoryBrandService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 管理端,分类品牌接口
 *
 * @author pikachu
 * @since 2020-02-27 15:18:56
 */
@RestController
@Tag(name = "管理端,分类品牌接口")
@RequestMapping("/manager/goods/categoryBrand")
public class CategoryBrandManagerController {

    /**
     * 规格品牌管理
     */
    @Autowired
    private CategoryBrandService categoryBrandService;

    @Operation(summary = "根据品牌ID获取分类列表")
    @Parameter(name = "brandId", description = "品牌ID", required = true)
    @GetMapping("/{brandId}")
    public ResultMessage<List<Category>> getBrandCategory(@PathVariable String brandId) {
        return ResultUtil.data(categoryBrandService.getBrandCategoryList(brandId));
    }

    @Operation(summary = "保存品牌分类关联")
    @Parameter(name = "brandId", description = "品牌ID", required = true)
    @PostMapping("/{brandId}")
    public ResultMessage<Object> saveBrandCategory(@PathVariable String brandId, @RequestBody List<String> categoryIds) {
        categoryBrandService.saveBrandCategoryList(brandId, categoryIds);
        return ResultUtil.success();
    }

}
