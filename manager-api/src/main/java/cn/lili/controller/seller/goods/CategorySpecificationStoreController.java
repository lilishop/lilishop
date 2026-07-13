package cn.lili.controller.seller.goods;

import cn.lili.modules.goods.entity.dos.Specification;
import cn.lili.modules.goods.service.CategorySpecificationService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * 店铺端,商品分类规格接口
 *
 * @author pikachu
 * @since 2020-02-27 15:18:56
 */
@RestController
@Tag(name = "店铺端,商品分类规格接口")
@RequestMapping("/store/goods/categorySpec")
public class CategorySpecificationStoreController {
    @Autowired
    private CategorySpecificationService categorySpecificationService;


    @Operation(summary = "查询某分类下绑定的规格信息")
    @GetMapping("/{category_id}")
    @Parameter(name = "category_id", description = "分类id", required = true)
    public List<Specification> getCategorySpec(@PathVariable("category_id") String categoryId) {
        return categorySpecificationService.getCategorySpecList(categoryId);
    }


}
