package cn.lili.controller.goods;

import cn.lili.modules.goods.entity.vos.ParameterGroupVO;
import cn.lili.modules.goods.service.CategoryParameterGroupService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * 店铺端,分类绑定参数组管理接口
 *
 * @author pikachu
 * @since 2020-02-18 15:18:56
 */
@RestController
@Api(tags = "店铺端,分类绑定参数组管理接口")
@RequestMapping("/store/goods/category/parameters")
@Transactional(rollbackFor = Exception.class)
public class CategoryParameterGroupStoreController {


    @Autowired
    private CategoryParameterGroupService categoryParameterGroupService;

    @ApiOperation(value = "查询某分类下绑定的参数信息")
    @GetMapping(value = "/{category_id}")
    @ApiImplicitParam(name = "category_id", value = "分类id", required = true, dataType = "String", paramType = "path")
    public List<ParameterGroupVO> getCategoryParam(@PathVariable("category_id") String categoryId) {
        return categoryParameterGroupService.getCategoryParams(categoryId);
    }

}
