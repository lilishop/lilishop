package cn.lili.controller.permission;

import cn.lili.mybatis.util.PageUtil;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.permission.entity.dos.Role;
import cn.lili.modules.permission.service.RoleService;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
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
@Api(tags = "管理端,角色管理接口")
@RequestMapping("/manager/permission/role")
public class RoleManagerController {
    @Autowired
    private RoleService roleService;

    @PostMapping
    @ApiOperation(value = "添加")
    public ResultMessage<Role> add(Role role) {
        roleService.save(role);
        return ResultUtil.data(role);
    }

    @GetMapping
    @ApiOperation(value = "查询")
    public ResultMessage<Page> add(PageVO pageVo, Role role) {
        Page page = roleService.page(PageUtil.initPage(pageVo), PageUtil.initWrapper(role));
        return ResultUtil.data(page);
    }

    @PutMapping("/{roleId}")
    @ApiOperation(value = "编辑")
    public ResultMessage<Role> edit(@PathVariable String roleId, Role role) {
        role.setId(roleId);
        roleService.updateById(role);
        return ResultUtil.data(role);
    }

    @DeleteMapping(value = "/{ids}")
    @ApiOperation(value = "批量删除")
    public ResultMessage<Role> delByIds(@PathVariable List<String> ids) {
        roleService.deleteRoles(ids);
        return ResultUtil.success();
    }


}
