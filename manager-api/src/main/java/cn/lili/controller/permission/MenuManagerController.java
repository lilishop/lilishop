package cn.lili.controller.permission;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.utils.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.permission.entity.dos.Menu;
import cn.lili.modules.permission.entity.dto.MenuSearchParams;
import cn.lili.modules.permission.entity.vo.MenuVO;
import cn.lili.modules.permission.service.MenuService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;


/**
 * 管理端,菜单管理接口
 *
 * @author Chopper
 * @date 2020/11/20 12:07
 */
@RestController
@Api(tags = "管理端,菜单管理接口")
@RequestMapping("/manager/menu")
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class MenuManagerController {

    private final MenuService menuService;


    @ApiOperation(value = "搜索菜单")
    @GetMapping
    public ResultMessage<List<Menu>> searchPermissionList(MenuSearchParams searchParams) {
        return ResultUtil.data(menuService.searchList(searchParams));
    }

    @ApiOperation(value = "添加")
    @PostMapping
    public ResultMessage<Menu> add(Menu menu) {
        try {
            menuService.save(menu);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return ResultUtil.data(menu);
    }

    @ApiImplicitParam(name = "id", value = "菜单ID", required = true, paramType = "path", dataType = "String")
    @ApiOperation(value = "编辑")
    @PutMapping(value = "/{id}")
    public ResultMessage<Menu> edit(@PathVariable String id, Menu menu) {
        menu.setId(id);
        menuService.updateById(menu);
        return ResultUtil.data(menu);
    }

    @ApiOperation(value = "批量删除")
    @DeleteMapping(value = "/{ids}")
    public ResultMessage<Menu> delByIds(@PathVariable List<String> ids) {
        menuService.deleteIds(ids);
        return ResultUtil.success(ResultCode.SUCCESS);
    }

    @ApiOperation(value = "获取所有菜单")
    @GetMapping("/tree")
    public ResultMessage<List<MenuVO>> getAllMenuList() {
        return ResultUtil.data(menuService.tree());
    }

    @ApiOperation(value = "获取所有菜单")
    @GetMapping("/memberMenu")
    public ResultMessage<List<MenuVO>> memberMenu() {
        return ResultUtil.data(menuService.findUserTree());
    }
}
