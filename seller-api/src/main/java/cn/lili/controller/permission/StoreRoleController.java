package cn.lili.controller.permission;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.dos.StoreRole;
import cn.lili.modules.member.service.StoreRoleService;
import cn.lili.modules.permission.entity.dos.Role;
import cn.lili.modules.permission.service.RoleService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;


/**
 * 店铺端,角色管理接口
 *
 * @author Chopper
 * @since 2020/11/20 18:50
 */
@RestController
@Api(tags = "店铺端,店铺角色管理接口")
@RequestMapping("/store/role")
public class StoreRoleController {
    @Autowired
    private StoreRoleService storeRoleService;

    @PostMapping
    @ApiOperation(value = "添加角色")
    public ResultMessage<StoreRole> add(StoreRole storeRole) {
        storeRoleService.saveStoreRole(storeRole);
        return ResultUtil.data(storeRole);
    }

    @GetMapping
    @ApiOperation(value = "查询店铺角色")
    public ResultMessage<Page> page(PageVO pageVo, StoreRole storeRole) {
        storeRole.setStoreId(UserContext.getCurrentUser().getStoreId());
        Page page = storeRoleService.page(PageUtil.initPage(pageVo), PageUtil.initWrapper(storeRole));
        return ResultUtil.data(page);
    }

    @PutMapping("/{roleId}")
    @ApiOperation(value = "编辑店铺角色")
    public ResultMessage<StoreRole> edit(@PathVariable String roleId, StoreRole storeRole) {
        storeRole.setId(roleId);
        storeRoleService.update(storeRole);
        return ResultUtil.data(storeRole);
    }

    @DeleteMapping(value = "/{ids}")
    @ApiOperation(value = "批量删除店铺角色")
    public ResultMessage<Role> delByIds(@PathVariable List<String> ids) {
        storeRoleService.deleteRoles(ids);
        return ResultUtil.success();
    }


}
