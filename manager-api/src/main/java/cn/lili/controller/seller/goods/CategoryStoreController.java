package cn.lili.controller.seller.goods;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.vos.CategoryBrandVO;
import cn.lili.modules.goods.entity.vos.CategoryVO;
import cn.lili.modules.goods.service.CategoryBrandService;
import cn.lili.modules.goods.service.CategoryService;
import cn.lili.modules.store.service.StoreDetailService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.CacheConfig;
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
@Tag(name = "店铺端,商品分类接口")
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

    @Operation(summary = "获取店铺经营的分类")
    @GetMapping("/all")
    public ResultMessage<List<CategoryVO>> getListAll() {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        //获取店铺经营范围
        String goodsManagementCategory = storeDetailService.getStoreDetail(storeId).getGoodsManagementCategory();
        return ResultUtil.data(this.categoryService.getStoreCategory(goodsManagementCategory.split(",")));
    }

    @Operation(summary = "获取所选分类关联的品牌信息")
    @GetMapping("/{categoryId}/brands")
    @Parameter(name = "categoryId", description = "分类id", required = true)
    public List<CategoryBrandVO> queryBrands(@PathVariable String categoryId) {
        return this.categoryBrandService.getCategoryBrandList(categoryId);
    }

}
