package cn.lili.modules.permission.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.token.Token;
import cn.lili.common.utils.BeanUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.modules.permission.entity.dos.AdminUser;
import cn.lili.modules.permission.entity.dos.Department;
import cn.lili.modules.permission.entity.dos.Role;
import cn.lili.modules.permission.entity.dos.UserRole;
import cn.lili.modules.permission.entity.dto.AdminUserDTO;
import cn.lili.modules.permission.entity.vo.AdminUserVO;
import cn.lili.modules.permission.mapper.AdminUserMapper;
import cn.lili.modules.permission.service.*;
import cn.lili.modules.system.aspect.annotation.SystemLogPoint;
import cn.lili.modules.system.token.ManagerTokenGenerate;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * 用户业务层实现
 *
 * @author Chopper
 * @since 2020/11/17 3:46 下午
 */
@Slf4j
@Service
public class AdminUserServiceImpl extends ServiceImpl<AdminUserMapper, AdminUser> implements AdminUserService {
    @Autowired
    private UserRoleService userRoleService;
    @Autowired
    private RoleService roleService;
    @Autowired
    private DepartmentService departmentService;
    @Autowired
    private MenuService menuService;
    @Autowired
    private ManagerTokenGenerate managerTokenGenerate;
    /**
     * 角色长度
     */
    private final int rolesMaxSize = 10;

    @Override
    public IPage<AdminUserVO> adminUserPage(Page initPage, QueryWrapper<AdminUser> initWrapper) {
        Page<AdminUser> adminUserPage = page(initPage, initWrapper);
        List<Role> roles = roleService.list();
        List<Department> departments = departmentService.list();

        List<AdminUserVO> result = new ArrayList<>();

        adminUserPage.getRecords().forEach(adminUser -> {
            AdminUserVO adminUserVO = new AdminUserVO(adminUser);
            if (!CharSequenceUtil.isEmpty(adminUser.getDepartmentId())) {
                try {
                    adminUserVO.setDepartmentTitle(
                            departments.stream().filter
                                            (department -> department.getId().equals(adminUser.getDepartmentId()))
                                    .collect(Collectors.toList())
                                    .get(0)
                                    .getTitle()
                    );
                } catch (Exception e) {
                    log.error("填充部门信息异常", e);
                }
            }
            if (!CharSequenceUtil.isEmpty(adminUser.getRoleIds())) {
                try {
                    List<String> memberRoles = Arrays.asList(adminUser.getRoleIds().split(","));
                    adminUserVO.setRoles(
                            roles.stream().filter
                                            (role -> memberRoles.contains(role.getId()))
                                    .collect(Collectors.toList())
                    );
                } catch (Exception e) {
                    log.error("填充部门信息异常", e);
                }
            }
            result.add(adminUserVO);
        });
        Page<AdminUserVO> pageResult = new Page(adminUserPage.getCurrent(), adminUserPage.getSize(), adminUserPage.getTotal());
        pageResult.setRecords(result);
        return pageResult;

    }

    @Autowired
    private void setManagerTokenGenerate(ManagerTokenGenerate managerTokenGenerate) {
        this.managerTokenGenerate = managerTokenGenerate;
    }

    @Override
    @SystemLogPoint(description = "管理员登录", customerLog = "'管理员登录请求:'+#username")
    public Token login(String username, String password) {
        AdminUser adminUser = this.findByUsername(username);

        if (adminUser == null || !adminUser.getStatus()) {
            throw new ServiceException(ResultCode.USER_PASSWORD_ERROR);
        }
        if (!new BCryptPasswordEncoder().matches(password, adminUser.getPassword())) {
            throw new ServiceException(ResultCode.USER_PASSWORD_ERROR);
        }
        try {
            return managerTokenGenerate.createToken(adminUser, false);
        } catch (Exception e) {
            log.error("管理员登录错误", e);
        }
        return null;

    }


    @Override
    public Token refreshToken(String refreshToken) {
        return managerTokenGenerate.refreshToken(refreshToken);
    }


    @Override
    public AdminUser findByUsername(String username) {

        AdminUser user = getOne(new LambdaQueryWrapper<AdminUser>().eq(AdminUser::getUsername, username));

        if (user == null) {
            return null;
        }
        return user;
    }


    @Override
    @SystemLogPoint(description = "修改管理员", customerLog = "'修改管理员:'+#adminUser.username")
    @Transactional(rollbackFor = Exception.class)
    public boolean updateAdminUser(AdminUser adminUser, List<String> roles) {

        if (roles != null && !roles.isEmpty()) {

            if (roles.size() > rolesMaxSize) {
                throw new ServiceException(ResultCode.PERMISSION_BEYOND_TEN);
            }
            adminUser.setRoleIds(CharSequenceUtil.join(",", roles));

        } else {
            adminUser.setRoleIds("");
        }

        updateRole(adminUser.getId(), roles);
        this.updateById(adminUser);
        return true;
    }


    @Override
    public void editPassword(String password, String newPassword) {
        AuthUser tokenUser = UserContext.getCurrentUser();
        AdminUser user = this.getById(tokenUser.getId());
        if (!new BCryptPasswordEncoder().matches(password, user.getPassword())) {
            throw new ServiceException(ResultCode.USER_OLD_PASSWORD_ERROR);
        }
        String newEncryptPass = new BCryptPasswordEncoder().encode(newPassword);
        user.setPassword(newEncryptPass);
        this.updateById(user);
    }

    @Override
    public void resetPassword(List<String> ids) {
        LambdaQueryWrapper<AdminUser> lambdaQueryWrapper = new LambdaQueryWrapper();
        lambdaQueryWrapper.in(AdminUser::getId, ids);
        List<AdminUser> adminUsers = this.list(lambdaQueryWrapper);
        String password = StringUtils.md5("123456");
        String newEncryptPass = new BCryptPasswordEncoder().encode(password);
        if (null != adminUsers && !adminUsers.isEmpty()) {
            adminUsers.forEach(item -> item.setPassword(newEncryptPass));
            this.updateBatchById(adminUsers);
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveAdminUser(AdminUserDTO adminUser, List<String> roles) {
        AdminUser dbUser = new AdminUser();
        BeanUtil.copyProperties(adminUser, dbUser);
        dbUser.setPassword(new BCryptPasswordEncoder().encode(dbUser.getPassword()));
        if (roles.size() > rolesMaxSize) {
            throw new ServiceException(ResultCode.PERMISSION_BEYOND_TEN);
        }
        if (!roles.isEmpty()) {
            dbUser.setRoleIds(CharSequenceUtil.join(",", roles));
        }
        this.save(dbUser);
        dbUser = this.findByUsername(dbUser.getUsername());
        updateRole(dbUser.getId(), roles);
    }


    @Override
    public void deleteCompletely(List<String> ids) {
        //彻底删除超级管理员
        this.removeByIds(ids);

        //删除管理员角色
        QueryWrapper<UserRole> queryWrapper = new QueryWrapper<>();
        queryWrapper.in("user_id", ids);
        userRoleService.remove(queryWrapper);
    }

    /**
     * 更新用户角色
     *
     * @param userId 用户id
     * @param roles  角色id集合
     */
    private void updateRole(String userId, List<String> roles) {

        QueryWrapper queryWrapper = new QueryWrapper();
        queryWrapper.eq("user_id", userId);
        userRoleService.remove(queryWrapper);

        if (roles.isEmpty()) {
            return;
        }
        List<UserRole> userRoles = new ArrayList<>(roles.size());
        roles.forEach(id -> userRoles.add(new UserRole(userId, id)));
        userRoleService.updateUserRole(userId, userRoles);
    }
}
