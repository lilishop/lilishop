package cn.lili.controller.seller.permission;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.dos.StoreRole;
import cn.lili.modules.member.service.StoreRoleService;
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
 * 店铺端,角色管理接口
 *
 * @author Chopper
 * @since 2020/11/20 18:50
 */
@RestController
@Tag(name = "店铺端,店铺角色管理接口")
@RequestMapping("/store/role")
public class StoreRoleController {
    @Autowired
    private StoreRoleService storeRoleService;

    @Operation(description = "添加角色")
    @PostMapping
    public ResultMessage<StoreRole> add(StoreRole storeRole) {
        storeRoleService.saveStoreRole(storeRole);
        return ResultUtil.data(storeRole);
    }

    @Operation(description = "查询店铺角色")
    @GetMapping
    public ResultMessage<Page> page(PageVO pageVo, StoreRole storeRole) {
        storeRole.setStoreId(UserContext.getCurrentUser().getStoreId());
        Page page = storeRoleService.getRolePage(pageVo, storeRole);
        return ResultUtil.data(page);
    }

    @Operation(description = "编辑店铺角色")
    @Parameter(name = "roleId", description = "角色ID", required = true)
    @PutMapping("/{roleId}")
    public ResultMessage<StoreRole> edit(@PathVariable String roleId, StoreRole storeRole) {
        storeRole.setId(roleId);
        storeRoleService.update(storeRole);
        return ResultUtil.data(storeRole);
    }

    @Operation(description = "批量删除店铺角色")
    @Parameter(name = "ids", description = "角色ID列表", required = true)
    @DeleteMapping("/{ids}")
    public ResultMessage<Role> delByIds(@PathVariable List<String> ids) {
        storeRoleService.deleteRoles(ids);
        return ResultUtil.success();
    }


}
