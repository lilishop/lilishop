package cn.lili.controller.goods;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.vos.CategoryBrandVO;
import cn.lili.modules.goods.service.CategoryBrandService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
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
@Api(tags = "管理端,分类品牌接口")
@RequestMapping("/manager/goods/categoryBrand")
public class CategoryBrandManagerController {

    /**
     * 规格品牌管理
     */
    @Autowired
    private CategoryBrandService categoryBrandService;

    @ApiOperation(value = "查询某分类下绑定的品牌信息")
    @ApiImplicitParam(name = "categoryId", value = "分类id", required = true, dataType = "String", paramType = "path")
    @GetMapping(value = "/{categoryId}")
    public ResultMessage<List<CategoryBrandVO>> getCategoryBrand(@PathVariable String categoryId) {
        return ResultUtil.data(categoryBrandService.getCategoryBrandList(categoryId));
    }

    @ApiOperation(value = "保存某分类下绑定的品牌信息")
    @PostMapping(value = "/{categoryId}")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "categoryId", value = "分类id", required = true, paramType = "path", dataType = "String"),
            @ApiImplicitParam(name = "categoryBrands", value = "品牌id数组", required = true, paramType = "query", dataType = "String[]")
    })
    public ResultMessage<Object> saveCategoryBrand(@PathVariable String categoryId, @RequestParam List<String> categoryBrands) {
        categoryBrandService.saveCategoryBrandList(categoryId,categoryBrands);
        return ResultUtil.success();
    }

}
