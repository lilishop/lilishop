package cn.lili.controller.manager.permission;

import cn.lili.common.aop.annotation.DemoSite;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.permission.entity.dos.Menu;
import cn.lili.modules.permission.entity.dto.MenuSearchParams;
import cn.lili.modules.permission.entity.vo.MenuVO;
import cn.lili.modules.permission.service.MenuService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;


/**
 * 管理端,菜单管理接口
 *
 * @author Chopper
 * @since 2020/11/20 12:07
 */
@Slf4j
@RestController
@Tag(name = "管理端,菜单管理接口")
@RequestMapping("/manager/permission/menu")
public class MenuManagerController {

    @Autowired
    private MenuService menuService;

    @Operation(description = "搜索菜单")
    @Parameter(name = "searchParams", description = "查询参数")
    @GetMapping
    public ResultMessage<List<Menu>> searchPermissionList(MenuSearchParams searchParams) {
        return ResultUtil.data(menuService.searchList(searchParams));
    }

    @Operation(description = "添加菜单")
    @Parameter(name = "menu", description = "菜单参数", required = true)
    @PostMapping
    @DemoSite
    public ResultMessage<Menu> add(Menu menu) {
        try {
            menuService.saveOrUpdateMenu(menu);
        } catch (Exception e) {
            log.error("添加菜单错误", e);
        }
        return ResultUtil.data(menu);
    }

    @Operation(description = "编辑菜单")
    @Parameter(name = "id", description = "菜单ID", required = true)
    @PutMapping("/{id}")
    @DemoSite
    public ResultMessage<Menu> edit(@PathVariable String id, Menu menu) {
        menu.setId(id);
        menuService.saveOrUpdateMenu(menu);
        return ResultUtil.data(menu);
    }

    @Operation(description = "批量删除菜单")
    @Parameter(name = "ids", description = "菜单ID列表", required = true)
    @DeleteMapping("/{ids}")
    @DemoSite
    public ResultMessage<Menu> delByIds(@PathVariable List<String> ids) {
        menuService.deleteIds(ids);
        return ResultUtil.success();
    }

    @Operation(description = "获取所有菜单")
    @GetMapping("/tree")
    public ResultMessage<List<MenuVO>> getAllMenuList() {
        return ResultUtil.data(menuService.tree());
    }

    @Operation(description = "获取所有菜单--根据当前用户角色")
    @GetMapping("/memberMenu")
    public ResultMessage<List<MenuVO>> memberMenu() {
        return ResultUtil.data(menuService.findUserTree());
    }
}
