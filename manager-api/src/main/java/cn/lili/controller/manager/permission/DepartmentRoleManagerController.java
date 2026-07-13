package cn.lili.controller.manager.permission;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.permission.entity.dos.DepartmentRole;
import cn.lili.modules.permission.service.DepartmentRoleService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;


/**
 * 管理端,部门角色接口
 *
 * @author Chopper
 * @since 2020/11/22 14:05
 */
@RestController
@Tag(name = "管理端,部门角色接口")
@RequestMapping("/manager/permission/departmentRole")
public class DepartmentRoleManagerController {
    @Autowired
    private DepartmentRoleService departmentRoleService;

    @Operation(description = "查看部门拥有的角色")
    @Parameter(name = "departmentId", description = "部门ID", required = true)
    @GetMapping("/{departmentId}")
    public ResultMessage<List<DepartmentRole>> get(@PathVariable String departmentId) {
        return ResultUtil.data(departmentRoleService.listByDepartmentId(departmentId));
    }

    @Operation(description = "更新部门角色")
    @Parameter(name = "departmentId", description = "部门ID", required = true)
    @Parameter(name = "departmentRole", description = "部门角色参数", required = true)
    @PutMapping("/{departmentId}")
    public ResultMessage<DepartmentRole> update(@PathVariable String departmentId, @RequestBody List<DepartmentRole> departmentRole) {

        departmentRoleService.updateByDepartmentId(departmentId, departmentRole);
        return ResultUtil.success();
    }

}
