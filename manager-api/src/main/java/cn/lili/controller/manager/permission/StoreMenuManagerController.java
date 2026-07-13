package cn.lili.controller.manager.permission;

import cn.lili.common.aop.annotation.DemoSite;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.dos.StoreMenu;
import cn.lili.modules.member.entity.vo.StoreMenuVO;
import cn.lili.modules.member.service.StoreMenuService;
import cn.lili.modules.permission.entity.dos.Menu;
import cn.lili.modules.permission.entity.dto.MenuSearchParams;
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
@RequestMapping("/manager/permission/storeMenu")
public class StoreMenuManagerController {

    @Autowired
    private StoreMenuService storeMenuService;

    @Operation(description = "搜索菜单")
    @Parameter(name = "searchParams", description = "菜单搜索参数")
    @GetMapping
    public ResultMessage<List<StoreMenu>> searchPermissionList(MenuSearchParams searchParams) {
        return ResultUtil.data(storeMenuService.searchList(searchParams));
    }

    @Operation(description = "添加")
    @Parameter(name = "menu", description = "菜单对象")
    @PostMapping
    @DemoSite
    public ResultMessage<StoreMenu> add(StoreMenu menu) {
        try {
            storeMenuService.saveOrUpdateMenu(menu);
        } catch (Exception e) {
            log.error("添加菜单错误", e);
        }
        return ResultUtil.data(menu);
    }

    @Operation(description = "编辑")
    @Parameter(name = "id", description = "菜单ID", required = true)
    @Parameter(name = "menu", description = "菜单对象")
    @PutMapping("/{id}")
    @DemoSite
    public ResultMessage<StoreMenu> edit(@PathVariable String id, StoreMenu menu) {
        menu.setId(id);
        storeMenuService.saveOrUpdateMenu(menu);
        return ResultUtil.data(menu);
    }

    @Operation(description = "批量删除")
    @Parameter(name = "ids", description = "菜单ID列表", required = true)
    @DeleteMapping("/{ids}")
    @DemoSite
    public ResultMessage<Menu> delByIds(@PathVariable List<String> ids) {
        storeMenuService.deleteIds(ids);
        return ResultUtil.success();
    }

    @Operation(description = "获取所有菜单")
    @GetMapping("/tree")
    public ResultMessage<List<StoreMenuVO>> getAllMenuList() {
        return ResultUtil.data(storeMenuService.tree());
    }

}
