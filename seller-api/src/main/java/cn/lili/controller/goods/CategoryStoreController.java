package cn.lili.controller.goods;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.vos.CategoryBrandVO;
import cn.lili.modules.goods.entity.vos.CategoryVO;
import cn.lili.modules.goods.service.CategoryBrandService;
import cn.lili.modules.goods.service.CategoryService;
import cn.lili.modules.store.service.StoreDetailService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;
import java.util.Objects;

/**
 * 店铺端,商品分类接口
 *
 * @author Chopper
 * @since 2021/2/20 2:26 下午
 */
@RestController
@Api(tags = "店铺端,商品分类接口")
@RequestMapping("/store/goods/category")
@CacheConfig(cacheNames = "category")
public class CategoryStoreController {

    /**
     * 分类
     */
    @Autowired
    private CategoryService categoryService;
    /**
     * 分类品牌
     */
    @Autowired
    private CategoryBrandService categoryBrandService;
    /**
     * 店铺详情
     */
    @Autowired
    private StoreDetailService storeDetailService;

    @ApiOperation(value = "获取店铺经营的分类")
    @GetMapping(value = "/all")
    public ResultMessage<List<CategoryVO>> getListAll() {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        //获取店铺经营范围
        String goodsManagementCategory = storeDetailService.getStoreDetail(storeId).getGoodsManagementCategory();
        return ResultUtil.data(this.categoryService.getStoreCategory(goodsManagementCategory.split(",")));
    }

    @ApiOperation(value = "获取所选分类关联的品牌信息")
    @GetMapping(value = "/{categoryId}/brands")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "categoryId", value = "分类id", required = true, paramType = "path"),
    })
    public List<CategoryBrandVO> queryBrands(@PathVariable String categoryId) {
        return this.categoryBrandService.getCategoryBrandList(categoryId);
    }

}
