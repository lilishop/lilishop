package cn.lili.controller.passport;

import cn.lili.common.aop.annotation.DemoSite;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.security.token.Token;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.permission.entity.dos.AdminUser;
import cn.lili.modules.permission.entity.dto.AdminUserDTO;
import cn.lili.modules.permission.entity.vo.AdminUserVO;
import cn.lili.modules.permission.service.AdminUserService;
import cn.lili.modules.permission.service.DepartmentService;
import cn.lili.modules.verification.entity.enums.VerificationEnums;
import cn.lili.modules.verification.service.VerificationService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
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
@RequestMapping("/manager/passport/user")
@Validated
public class AdminUserManagerController {
    @Autowired
    private AdminUserService adminUserService;
    @Autowired
    private DepartmentService departmentService;
    /**
     * 会员
     */
    @Autowired
    private MemberService memberService;

    @Autowired
    private VerificationService verificationService;

    @PostMapping(value = "/login")
    @ApiOperation(value = "登录管理员")
    public ResultMessage<Token> login(@NotNull(message = "用户名不能为空") @RequestParam String username,
                                      @NotNull(message = "密码不能为空") @RequestParam String password,
                                      @RequestHeader String uuid) {
        if (verificationService.check(uuid, VerificationEnums.LOGIN)) {
            return ResultUtil.data(adminUserService.login(username, password));
        } else {
            throw new ServiceException(ResultCode.VERIFICATION_ERROR);
        }
    }

    @ApiOperation(value = "注销接口")
    @PostMapping("/logout")
    public ResultMessage<Object> logout() {
        this.memberService.logout(UserEnums.MANAGER);
        return ResultUtil.success();
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
        throw new ServiceException(ResultCode.USER_NOT_LOGIN);
    }

    @PutMapping(value = "/edit")
    @ApiOperation(value = "修改用户自己资料", notes = "用户名密码不会修改")
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

    @PutMapping(value = "/admin/edit")
    @ApiOperation(value = "超级管理员修改其他管理员资料")
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
    @PutMapping(value = "/editPassword")
    @ApiOperation(value = "修改密码")
    @DemoSite
    public ResultMessage<Object> editPassword(String password, String newPassword) {
        adminUserService.editPassword(password, newPassword);
        return ResultUtil.success(ResultCode.USER_EDIT_SUCCESS);
    }

    @PostMapping(value = "/resetPassword/{ids}")
    @ApiOperation(value = "重置密码")
    @DemoSite
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
    public ResultMessage<Object> register(@Valid AdminUserDTO adminUser,
                                          @RequestParam(required = false) List<String> roles) {
        int rolesMaxSize=10;
        try {
            if (roles != null && roles.size() >= rolesMaxSize) {
                throw new ServiceException(ResultCode.PERMISSION_BEYOND_TEN);
            }
            adminUserService.saveAdminUser(adminUser, roles);
        } catch (Exception e) {
            log.error("添加用户错误", e);
        }
        return ResultUtil.success();
    }

    @PutMapping(value = "/enable/{userId}")
    @ApiOperation(value = "禁/启 用 用户")
    @DemoSite
    public ResultMessage<Object> disable(@ApiParam("用户唯一id标识") @PathVariable String userId, Boolean status) {
        AdminUser user = adminUserService.getById(userId);
        if (user == null) {
            throw new ServiceException(ResultCode.USER_NOT_EXIST);
        }
        user.setStatus(status);
        adminUserService.updateById(user);
        return ResultUtil.success();
    }

    @DeleteMapping(value = "/{ids}")
    @ApiOperation(value = "批量通过ids删除")
    @DemoSite
    public ResultMessage<Object> delAllByIds(@PathVariable List<String> ids) {
        adminUserService.deleteCompletely(ids);
        return ResultUtil.success();
    }

}
