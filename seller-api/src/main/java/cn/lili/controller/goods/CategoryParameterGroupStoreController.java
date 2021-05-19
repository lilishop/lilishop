package cn.lili.controller.goods;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.dos.CategoryParameterGroup;
import cn.lili.modules.goods.entity.dos.Parameters;
import cn.lili.modules.goods.entity.vos.ParameterGroupVO;
import cn.lili.modules.goods.service.CategoryParameterGroupService;
import cn.lili.modules.goods.service.ParametersService;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 店铺端,分类绑定参数组管理接口
 *
 * @author pikachu
 * @date 2020-02-18 15:18:56
 */
@RestController
@Api(tags = "店铺端,分类绑定参数组管理接口")
@RequestMapping("/store/goods/category/parameters")
@Transactional(rollbackFor = Exception.class)
public class CategoryParameterGroupStoreController {
    @Autowired
    private ParametersService parametersService;
    @Autowired
    private CategoryParameterGroupService categoryParameterGroupService;

    @ApiOperation(value = "查询某分类下绑定的参数信息")
    @GetMapping(value = "/{category_id}")
    @ApiImplicitParam(name = "category_id", value = "分类id", required = true, dataType = "String", paramType = "path")
    public List<ParameterGroupVO> getCategoryParam(@PathVariable("category_id") String categoryId) {
        return categoryParameterGroupService.getCategoryParams(categoryId);
    }

    @ApiOperation(value = "编辑或更新数据")
    @PostMapping(value = "/save")
    public ResultMessage<CategoryParameterGroup> saveOrUpdate(CategoryParameterGroup categoryParameterGroup) {

        categoryParameterGroupService.save(categoryParameterGroup);
        return ResultUtil.data(categoryParameterGroup);
    }

    @ApiOperation(value = "通过id删除参数组")
    @DeleteMapping(value = "/{id}")
    public ResultMessage<Object> delAllByIds(@PathVariable String id) {
        //删除参数
        parametersService.remove(new QueryWrapper<Parameters>().eq("group_id", id));
        //删除参数组
        categoryParameterGroupService.removeById(id);
        return ResultUtil.success();
    }

}
