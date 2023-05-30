package cn.lili.controller.goods;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.dos.CategorySpecification;
import cn.lili.modules.goods.entity.dos.Specification;
import cn.lili.modules.goods.service.CategorySpecificationService;
import cn.lili.modules.goods.service.SpecificationService;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.ArrayList;
import java.util.List;

/**
 * 管理端,商品分类规格接口
 *
 * @author pikachu
 * @since 2020-02-27 15:18:56
 */
@RestController
@Api(tags = "管理端,商品分类规格接口")
@RequestMapping("/manager/goods/categorySpec")
public class CategorySpecificationManagerController {

    /**
     * 分类规格
     */
    @Autowired
    private CategorySpecificationService categorySpecificationService;

    /**
     * 规格
     */
    @Autowired
    private SpecificationService specificationService;


    @ApiOperation(value = "查询某分类下绑定的规格信息")
    @GetMapping(value = "/{categoryId}")
    @ApiImplicitParam(name = "categoryId", value = "分类id", required = true, dataType = "String", paramType = "path")
    public List<Specification> getCategorySpec(@PathVariable String categoryId) {
        return categorySpecificationService.getCategorySpecList(categoryId);
    }

    @ApiOperation(value = "查询某分类下绑定的规格信息,商品操作使用")
    @GetMapping(value = "/goods/{categoryId}")
    @ApiImplicitParam(name = "categoryId", value = "分类id", required = true, dataType = "String", paramType = "path")
    public List<Specification> getSpec(@PathVariable String categoryId) {
        return specificationService.list();
    }


    @ApiOperation(value = "保存某分类下绑定的规格信息")
    @PostMapping(value = "/{categoryId}")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "categoryId", value = "分类id", required = true, paramType = "path", dataType = "String"),
            @ApiImplicitParam(name = "categorySpecs", value = "规格id数组", required = true, paramType = "query", dataType = "String[]")
    })
    public ResultMessage<String> saveCategoryBrand(@PathVariable String categoryId,
                                                   @RequestParam String[] categorySpecs) {
        //删除分类规格绑定信息
        this.categorySpecificationService.remove(new QueryWrapper<CategorySpecification>().eq("category_id", categoryId));
        //绑定规格信息
        if (categorySpecs != null && categorySpecs.length > 0) {
            List<CategorySpecification> categorySpecifications = new ArrayList<>();
            for (String categorySpec : categorySpecs) {
                categorySpecifications.add(new CategorySpecification(categoryId, categorySpec));
            }
            categorySpecificationService.saveBatch(categorySpecifications);
        }
        return ResultUtil.success();
    }

}
