package cn.lili.controller.goods;


import cn.lili.modules.goods.entity.dos.Specification;
import cn.lili.modules.goods.service.CategorySpecificationService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;


/**
 * 店铺端,规格管理接口
 *
 * @author pikachu
 * @since 2020-02-18 15:18:56
 */
@RestController
@Api(tags = "店铺端,规格接口")
@RequestMapping("/store/goods/spec")
public class SpecificationStoreController {

    @Autowired
    private CategorySpecificationService categorySpecificationService;

    @GetMapping(value = "/{categoryId}")
    @ApiOperation(value = "获取分类规格")
    public List<Specification> getSpecifications(@PathVariable String categoryId) {
        return categorySpecificationService.getCategorySpecList(categoryId);
    }

}
