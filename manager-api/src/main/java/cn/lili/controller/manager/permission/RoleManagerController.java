package cn.lili.controller.manager.permission;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.permission.entity.dos.Role;
import cn.lili.modules.permission.service.RoleService;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;


/**
 * 管理端,角色管理接口
 *
 * @author Chopper
 * @since 2020/11/20 18:50
 */
@RestController
@Tag(name = "管理端,角色管理接口")
@RequestMapping("/manager/permission/role")
public class RoleManagerController {
    @Autowired
    private RoleService roleService;

    @Operation(description = "添加角色")
    @PostMapping
    public ResultMessage<Role> add(Role role) {
        roleService.save(role);
        return ResultUtil.data(role);
    }

    @Operation(description = "查询角色")
    @GetMapping
    @Parameter(name = "pageVo", description = "分页参数")
    @Parameter(name = "role", description = "查询参数")
    public ResultMessage<Page> add(PageVO pageVo, Role role) {
        Page page = roleService.getRolePage(pageVo, role);
        return ResultUtil.data(page);
    }

    @Operation(description = "编辑角色")
    @PutMapping("/{roleId}")
    @Parameter(name = "roleId", description = "角色ID")
    @Parameter(name = "role", description = "角色信息")
    public ResultMessage<Role> edit(@PathVariable String roleId, Role role) {
        role.setId(roleId);
        roleService.updateById(role);
        return ResultUtil.data(role);
    }

    @Operation(description = "删除角色")
    @DeleteMapping("/{ids}")
    @Parameter(name = "ids", description = "角色ID列表")
    public ResultMessage<Role> delByIds(@PathVariable List<String> ids) {
        roleService.deleteRoles(ids);
        return ResultUtil.success();
    }


}
