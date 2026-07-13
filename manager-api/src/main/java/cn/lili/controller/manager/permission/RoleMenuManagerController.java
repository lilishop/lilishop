package cn.lili.controller.manager.permission;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.permission.entity.dos.RoleMenu;
import cn.lili.modules.permission.service.RoleMenuService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;


/**
 * 管理端,角色菜单接口
 *
 * @author Chopper
 * @since 2020/11/22 11:40
 */
@RestController
@Tag(name = "管理端,角色菜单接口")
@RequestMapping("/manager/permission/roleMenu")
public class RoleMenuManagerController {
    @Autowired
    private RoleMenuService roleMenuService;

    @Operation(description = "查看某角色拥有到菜单")
    @GetMapping("/{roleId}")
    @Parameter(name = "roleId", description = "角色ID")
    public ResultMessage<List<RoleMenu>> get(@PathVariable String roleId) {
        return ResultUtil.data(roleMenuService.findByRoleId(roleId));
    }

    @Operation(description = "保存角色菜单")
    @PostMapping("/{roleId}")
    @Parameter(name = "roleId", description = "角色ID")
    @Parameter(name = "roleMenus", description = "角色菜单列表")
    public ResultMessage save(@PathVariable String roleId, @RequestBody List<RoleMenu> roleMenus) {
        roleMenuService.updateRoleMenu(roleId, roleMenus);
        return ResultUtil.success();
    }

}
