package cn.lili.controller.permission;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.permission.entity.dos.DepartmentRole;
import cn.lili.modules.permission.service.DepartmentRoleService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
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
@Api(tags = "管理端,部门角色接口")
@RequestMapping("/manager/permission/departmentRole")
public class DepartmentRoleManagerController {
    @Autowired
    private DepartmentRoleService departmentRoleService;

    @GetMapping(value = "/{departmentId}")
    @ApiOperation(value = "查看部门拥有的角色")
    public ResultMessage<List<DepartmentRole>> get(@PathVariable String departmentId) {
        return ResultUtil.data(departmentRoleService.listByDepartmentId(departmentId));
    }

    @PutMapping("/{departmentId}")
    @ApiOperation(value = "更新部门角色")
    public ResultMessage<DepartmentRole> update(@PathVariable String departmentId, @RequestBody List<DepartmentRole> departmentRole) {

        departmentRoleService.updateByDepartmentId(departmentId, departmentRole);
        return ResultUtil.success();
    }

}
