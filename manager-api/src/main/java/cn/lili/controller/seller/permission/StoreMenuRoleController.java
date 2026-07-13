package cn.lili.controller.seller.permission;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.dos.StoreMenuRole;
import cn.lili.modules.member.service.StoreMenuRoleService;
import cn.lili.modules.permission.entity.dos.RoleMenu;
import cn.lili.modules.permission.service.RoleMenuService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;


/**
 * 店铺端,角色菜单接口
 *
 * @author Chopper
 * @since 2020/11/22 11:40
 */
@RestController
@Tag(name = "店铺端,角色菜单接口")
@RequestMapping("/store/roleMenu")
public class StoreMenuRoleController {
    @Autowired
    private StoreMenuRoleService storeMenuRoleService;

    @Operation(description = "查看某角色拥有到菜单")
    @Parameter(name = "roleId", description = "角色ID", required = true)
    @GetMapping("/{roleId}")
    public ResultMessage<List<StoreMenuRole>> get(@PathVariable String roleId) {
        return ResultUtil.data(storeMenuRoleService.findByRoleId(roleId));
    }

    @Operation(description = "保存角色菜单")
    @Parameter(name = "roleId", description = "角色ID", required = true)
    @PostMapping("/{roleId}")
    public ResultMessage save(@PathVariable String roleId, @RequestBody List<StoreMenuRole> roleMenus) {
        storeMenuRoleService.updateRoleMenu(roleId, roleMenus);
        return ResultUtil.success();
    }

}
