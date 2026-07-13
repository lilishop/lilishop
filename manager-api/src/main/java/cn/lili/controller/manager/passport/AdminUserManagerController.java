package cn.lili.controller.manager.passport;

import cn.lili.common.aop.annotation.DemoSite;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.security.token.Token;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.permission.entity.dos.AdminUser;
import cn.lili.modules.permission.entity.dto.AdminUserDTO;
import cn.lili.modules.permission.entity.vo.AdminUserVO;
import cn.lili.modules.permission.service.AdminUserService;
import cn.lili.modules.verification.entity.enums.VerificationEnums;
import cn.lili.modules.verification.service.VerificationService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotNull;
import java.util.ArrayList;
import java.util.List;


/**
 * 管理员接口
 *
 * @author Chopper
 * @since 2020/11/16 10:57
 */
@Slf4j
@RestController
@Tag(name = "管理员")
@RequestMapping("/manager/passport/user")
@Validated
public class AdminUserManagerController {
    @Autowired
    private AdminUserService adminUserService;
    /**
     * 客户
     */
    @Autowired
    private MemberService memberService;

    @Autowired
    private VerificationService verificationService;

    @PostMapping("/login")
    @Operation(description = "登录管理员")
    @Parameter(name = "username", description = "用户名", required = true)
    @Parameter(name = "password", description = "密码", required = true)
    @Parameter(name = "uuid", description = "验证码uuid", required = true)
    public ResultMessage<Token> login(@NotNull(message = "用户名不能为空") @RequestParam String username,
                                      @NotNull(message = "密码不能为空") @RequestParam String password,
                                      @RequestHeader String uuid) {
        if (verificationService.check(uuid, VerificationEnums.LOGIN)) {
            return ResultUtil.data(adminUserService.login(username, password));
        } else {
            throw new ServiceException(ResultCode.VERIFICATION_ERROR);
        }
    }

    @Operation(description = "注销接口")
    @PostMapping("/logout")
    public ResultMessage<Object> logout() {
        this.memberService.logout(UserEnums.MANAGER);
        return ResultUtil.success();
    }

    @Operation(description = "刷新token")
    @GetMapping("/refresh/{refreshToken}")
    public ResultMessage<Object> refreshToken(@NotNull(message = "刷新token不能为空") @PathVariable String refreshToken) {
        return ResultUtil.data(this.adminUserService.refreshToken(refreshToken));
    }


    @GetMapping("/info")
    @Operation(description = "获取当前登录用户接口")
    public ResultMessage<AdminUser> getUserInfo() {
        AuthUser tokenUser = UserContext.getCurrentUser();
        if (tokenUser != null) {
            AdminUser adminUser = adminUserService.findByUsername(tokenUser.getUsername());
            adminUser.setPassword(null);
            return ResultUtil.data(adminUser);
        }
        throw new ServiceException(ResultCode.USER_NOT_LOGIN);
    }

    @PutMapping("/edit")
    @Operation(description = "修改用户自己资料")
    public ResultMessage<Object> editOwner(AdminUser adminUser) {

        AuthUser tokenUser = UserContext.getCurrentUser();
        if (tokenUser != null) {
            //查询当前管理员
            AdminUser oldAdminUser = adminUserService.findByUsername(tokenUser.getUsername());
            oldAdminUser.setAvatar(adminUser.getAvatar());
            oldAdminUser.setNickName(adminUser.getNickName());
            if (!adminUserService.updateById(oldAdminUser)) {
                throw new ServiceException(ResultCode.USER_EDIT_ERROR);
            }
            return ResultUtil.success(ResultCode.USER_EDIT_SUCCESS);
        }
        throw new ServiceException(ResultCode.USER_NOT_LOGIN);
    }

    @PutMapping("/admin/edit")
    @Operation(description = "超级管理员修改其他管理员资料")
    @DemoSite
    public ResultMessage<Object> edit(@Valid AdminUser adminUser,
                                      @RequestParam(required = false) List<String> roles) {
        if (!adminUserService.updateAdminUser(adminUser, roles)) {
            throw new ServiceException(ResultCode.USER_EDIT_ERROR);
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
    @PutMapping("/editPassword")
    @Operation(description = "修改密码")
    @DemoSite
    public ResultMessage<Object> editPassword(String password, String newPassword) {
        adminUserService.editPassword(password, newPassword);
        return ResultUtil.success(ResultCode.USER_EDIT_SUCCESS);
    }

    @PostMapping("/resetPassword/{ids}")
    @Operation(description = "重置密码")
    @DemoSite
    public ResultMessage<Object> resetPassword(@PathVariable List<String> ids) {
        adminUserService.resetPassword(ids);
        return ResultUtil.success(ResultCode.USER_EDIT_SUCCESS);
    }

    @GetMapping("/getByCondition")
    @Operation(description = "多条件分页获取用户列表")
    public ResultMessage<IPage<AdminUserVO>> getByCondition(AdminUserDTO user,
                                                            SearchVO searchVo,
                                                            PageVO pageVo) {
        IPage<AdminUserVO> page = adminUserService.adminUserPage(user, searchVo, pageVo);

        return ResultUtil.data(page);
    }


    @PostMapping("/register")
    @Operation(description = "添加用户")
    public ResultMessage<Object> register(@Valid AdminUserDTO adminUser,
                                          @RequestParam(required = false) List<String> roles) {
        int rolesMaxSize = 10;
        try {
            if (roles != null && roles.size() >= rolesMaxSize) {
                throw new ServiceException(ResultCode.PERMISSION_BEYOND_TEN);
            }
            adminUserService.saveAdminUser(adminUser, roles);
            return ResultUtil.success();
        } catch (Exception e) {
            log.error("添加用户错误", e);
            return ResultUtil.error(ResultCode.USER_ADD_ERROR);
        }
    }

    @PutMapping("/enable/{userId}")
    @Operation(description = "禁/启 用 用户")
    @DemoSite
    public ResultMessage<Object> disable(@Parameter(description = "用户唯一id标识") @PathVariable String userId, Boolean status) {
        AdminUser user = adminUserService.getById(userId);
        if (user == null) {
            throw new ServiceException(ResultCode.USER_NOT_EXIST);
        }
        user.setStatus(status);
        adminUserService.updateById(user);

        //登出用户
        if (Boolean.FALSE.equals(status)) {
            List<String> userIds = new ArrayList<>();
            userIds.add(userId);
            adminUserService.logout(userIds);
        }

        return ResultUtil.success();
    }

    @DeleteMapping("/{ids}")
    @Operation(description = "批量通过ids删除")
    @DemoSite
    public ResultMessage<Object> delAllByIds(@PathVariable List<String> ids) {
        adminUserService.deleteCompletely(ids);
        return ResultUtil.success();
    }

}
