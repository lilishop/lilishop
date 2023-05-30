package cn.lili.controller.permission;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.permission.entity.dos.UserRole;
import cn.lili.modules.permission.service.UserRoleService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;


/**
 * 管理端,管理员角色接口
 *
 * @author Chopper
 * @since 2020/11/22 11:53
 */
@RestController
@Api(tags = "管理端,管理员角色接口")
@RequestMapping("/manager/permission/userRole")
public class UserRoleManagerController {
    @Autowired
    private UserRoleService userRoleService;

    @GetMapping(value = "/{userId}")
    @ApiOperation(value = "查看管理员角色")
    public ResultMessage<UserRole> get(@PathVariable String userId) {
        UserRole userRole = userRoleService.getById(userId);
        return ResultUtil.data(userRole);
    }

    @PutMapping("/{userId}")
    @ApiOperation(value = "更新角色菜单")
    public ResultMessage<UserRole> update(@PathVariable String userId, List<UserRole> userRole) {
        userRoleService.updateUserRole(userId, userRole);
        return ResultUtil.success();
    }

}
