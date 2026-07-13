package cn.lili.controller.seller.goods;

import cn.lili.modules.goods.entity.dos.Parameters;
import cn.lili.modules.goods.service.ParametersService;
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
 * 店铺端,分类绑定参数管理接口
 *
 * @author pikachu
 * @since 2020-02-18 15:18:56
 */
@RestController
@Tag(name = "店铺端,分类绑定参数管理接口")
@RequestMapping("/store/goods/categoryParameters")
public class CategoryParameterGroupStoreController {

    @Autowired
    private ParametersService parametersService;

    @Operation(summary = "查询某分类下绑定的参数信息")
    @GetMapping("/{category_id}")
    @Parameter(name = "category_id", description = "分类id", required = true)
    public List<Parameters> getCategoryParam(@PathVariable("category_id") String categoryId) {
        return parametersService.getParametersByCategoryId(categoryId);
    }

}
