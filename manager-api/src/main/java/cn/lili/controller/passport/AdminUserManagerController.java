package cn.lili.controller.passport;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.token.Token;
import cn.lili.common.utils.PageUtil;
import cn.lili.common.utils.ResultUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.permission.entity.dos.AdminUser;
import cn.lili.modules.permission.entity.dto.AdminUserDTO;
import cn.lili.modules.permission.entity.vo.AdminUserVO;
import cn.lili.modules.permission.service.AdminUserService;
import cn.lili.modules.permission.service.DepartmentService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.constraints.NotNull;
import java.util.List;


/**
 * 管理员接口
 *
 * @author Chopper
 * @since 2020/11/16 10:57
 */
@Slf4j
@RestController
@Api(tags = "管理员")
@RequestMapping("/manager/user")
@Transactional
@Validated
public class AdminUserManagerController {
    @Autowired
    private AdminUserService adminUserService;
    @Autowired
    private DepartmentService departmentService;


    @GetMapping(value = "/login")
    @ApiOperation(value = "登录管理员")
    public ResultMessage<Token> login(String username, String password) {
        return ResultUtil.data(adminUserService.login(username, password));
    }


    @ApiOperation(value = "刷新token")
    @GetMapping("/refresh/{refreshToken}")
    public ResultMessage<Object> refreshToken(@NotNull(message = "刷新token不能为空") @PathVariable String refreshToken) {
        return ResultUtil.data(this.adminUserService.refreshToken(refreshToken));
    }


    @GetMapping(value = "/info")
    @ApiOperation(value = "获取当前登录用户接口")
    public ResultMessage<AdminUserVO> getUserInfo() {
        AuthUser tokenUser = UserContext.getCurrentUser();
        if (tokenUser != null) {
            AdminUserVO adminUser = new AdminUserVO(adminUserService.findByUsername(tokenUser.getUsername()));
            if (StringUtils.isNotEmpty(adminUser.getDepartmentId())) {
                adminUser.setDepartmentTitle(departmentService.getById(adminUser.getDepartmentId()).getTitle());
            }
            adminUser.setPassword(null);
            return ResultUtil.data(adminUser);
        }
        return ResultUtil.error(ResultCode.USER_NOT_LOGIN);
    }

    @PutMapping(value = "/edit")
    @ApiOperation(value = "修改用户自己资料", notes = "用户名密码不会修改")
    public ResultMessage<Object> editOwner(AdminUser adminUser) {

        AuthUser tokenUser = UserContext.getCurrentUser();
        if (tokenUser != null) {
            //查询当前管理员
            AdminUser adminUserDB = adminUserService.findByUsername(tokenUser.getUsername());
            adminUserDB.setAvatar(adminUser.getAvatar());
            adminUserDB.setNickName(adminUser.getNickName());
            if (!adminUserService.updateById(adminUserDB)) {
                return ResultUtil.error(ResultCode.USER_EDIT_ERROR);
            }
            return ResultUtil.success(ResultCode.USER_EDIT_SUCCESS);
        }
        return ResultUtil.error(ResultCode.USER_NOT_LOGIN);
    }

    @PutMapping(value = "/admin/edit")
    @ApiOperation(value = "超级管理员修改其他管理员资料")
    public ResultMessage<Object> edit(AdminUser adminUser,
                                      @RequestParam(required = false) List<String> roles) {
        if (!adminUserService.updateAdminUser(adminUser, roles)) {
            return ResultUtil.error(ResultCode.USER_EDIT_ERROR);
        }
        return ResultUtil.success(ResultCode.USER_EDIT_SUCCESS);
    }

    /**
     * 修改密码
     *
     * @param password
     * @param newPassword
     * @return
     */
    @PutMapping(value = "/editPassword")
    @ApiOperation(value = "修改密码")
    public ResultMessage<Object> editPassword(String password, String newPassword) {
        adminUserService.editPassword(password, newPassword);
        return ResultUtil.success(ResultCode.USER_EDIT_SUCCESS);
    }

    @PostMapping(value = "/resetPassword/{ids}")
    @ApiOperation(value = "重置密码")
    public ResultMessage<Object> resetPassword(@PathVariable List ids) {
        adminUserService.resetPassword(ids);
        return ResultUtil.success(ResultCode.USER_EDIT_SUCCESS);
    }

    @GetMapping
    @ApiOperation(value = "多条件分页获取用户列表")
    public ResultMessage<IPage<AdminUserVO>> getByCondition(AdminUserDTO user,
                                                            SearchVO searchVo,
                                                            PageVO pageVo) {
        IPage<AdminUserVO> page = adminUserService.adminUserPage(PageUtil.initPage(pageVo), PageUtil.initWrapper(user, searchVo));

        return ResultUtil.data(page);
    }


    @PostMapping
    @ApiOperation(value = "添加用户")
    public ResultMessage<Object> register(AdminUserDTO adminUser,
                                          @RequestParam(required = false) List<String> roles) {
        try {
            if (roles != null & roles.size() >= 10) {
                return ResultUtil.error(ResultCode.PERMISSION_BEYOND_TEN);
            }
            adminUserService.saveAdminUser(adminUser, roles);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return ResultUtil.success(ResultCode.SUCCESS);
    }

    @PutMapping(value = "/enable/{userId}")
    @ApiOperation(value = "禁/启 用 用户")
    public ResultMessage<Object> disable(@ApiParam("用户唯一id标识") @PathVariable String userId, Boolean status) {
        AdminUser user = adminUserService.getById(userId);
        if (user == null) {
            return ResultUtil.error(ResultCode.USER_NOT_EXIST);
        }
        user.setStatus(status);
        adminUserService.updateById(user);
        return ResultUtil.success(ResultCode.SUCCESS);
    }

    @DeleteMapping(value = "/{ids}")
    @ApiOperation(value = "批量通过ids删除")
    public ResultMessage<Object> delAllByIds(@PathVariable List<String> ids) {
        adminUserService.deleteCompletely(ids);
        return ResultUtil.success(ResultCode.SUCCESS);
    }

}
