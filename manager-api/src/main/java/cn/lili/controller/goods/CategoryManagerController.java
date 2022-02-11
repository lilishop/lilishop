package cn.lili.controller.goods;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.dos.Category;
import cn.lili.modules.goods.entity.vos.CategoryVO;
import cn.lili.modules.goods.service.CategoryService;
import cn.lili.modules.goods.service.GoodsService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import java.util.List;

/**
 * 管理端,商品分类接口
 *
 * @author pikachu
 * @since 2020-02-27 15:18:56
 */
@RestController
@Api(tags = "管理端,商品分类接口")
@RequestMapping("/manager/goods/category")
@CacheConfig(cacheNames = "category")
public class CategoryManagerController {

    /**
     * 分类
     */
    @Autowired
    private CategoryService categoryService;

    /**
     * 商品
     */
    @Autowired
    private GoodsService goodsService;

    @ApiOperation(value = "查询某分类下的全部子分类列表")
    @ApiImplicitParam(name = "parentId", value = "父id，顶级为0", required = true, dataType = "String", paramType = "path")
    @GetMapping(value = "/{parentId}/all-children")
    public ResultMessage<List<Category>> list(@PathVariable String parentId) {
        return ResultUtil.data(this.categoryService.dbList(parentId));
    }

    @ApiOperation(value = "查询全部分类列表")
    @GetMapping(value = "/allChildren")
    public ResultMessage<List<CategoryVO>> list() {
        return ResultUtil.data(this.categoryService.listAllChildren());
    }

    @PostMapping
    @ApiOperation(value = "添加商品分类")
    public ResultMessage<Category> saveCategory(@Valid Category category) {
        //非顶级分类
        if (category.getParentId() != null && !"0".equals(category.getParentId())) {
            Category parent = categoryService.getById(category.getParentId());
            if (parent == null) {
                throw new ServiceException(ResultCode.CATEGORY_PARENT_NOT_EXIST);
            }
            if (category.getLevel() >= 4) {
                throw new ServiceException(ResultCode.CATEGORY_BEYOND_THREE);
            }
        }
        if (categoryService.saveCategory(category)) {
            return ResultUtil.data(category);
        }
        throw new ServiceException(ResultCode.CATEGORY_SAVE_ERROR);
    }

    @PutMapping
    @ApiOperation(value = "修改商品分类")
    public ResultMessage<Category> updateCategory(@Valid CategoryVO category) {
        Category catTemp = categoryService.getById(category.getId());
        if (catTemp == null) {
            throw new ServiceException(ResultCode.CATEGORY_NOT_EXIST);
        }

        categoryService.updateCategory(category);
        return ResultUtil.data(category);
    }

    @DeleteMapping(value = "/{id}")
    @ApiImplicitParam(name = "id", value = "分类ID", required = true, paramType = "path", dataType = "String")
    @ApiOperation(value = "通过id删除分类")
    public ResultMessage<Category> delAllByIds(@NotNull @PathVariable String id) {
        Category category = new Category();
        category.setParentId(id);
        List<Category> list = categoryService.findByAllBySortOrder(category);
        if (list != null && !list.isEmpty()) {
            throw new ServiceException(ResultCode.CATEGORY_HAS_CHILDREN);

        }
        //查询某商品分类的商品数量
        long count = goodsService.getGoodsCountByCategory(id);
        if (count > 0) {
            throw new ServiceException(ResultCode.CATEGORY_HAS_GOODS);
        }
        categoryService.delete(id);
        return ResultUtil.success();
    }

    @PutMapping(value = "/disable/{id}")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "goodsId", value = "分类ID", required = true, paramType = "path", dataType = "String")
    })
    @ApiOperation(value = "后台 禁用/启用 分类")
    public ResultMessage<Object> disable(@PathVariable String id, @RequestParam Boolean enableOperations) {

        Category category = categoryService.getById(id);
        if (category == null) {
            throw new ServiceException(ResultCode.CATEGORY_NOT_EXIST);
        }
        categoryService.updateCategoryStatus(id, enableOperations);
        return ResultUtil.success();
    }

}