package cn.lili.modules.permission.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.permission.entity.dos.Role;
import cn.lili.modules.permission.mapper.RoleMapper;
import cn.lili.modules.permission.service.DepartmentRoleService;
import cn.lili.modules.permission.service.RoleMenuService;
import cn.lili.modules.permission.service.RoleService;
import cn.lili.modules.permission.service.UserRoleService;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * 角色业务层实现
 *
 * @author Chopper
 * @since 2020/11/17 3:50 下午
 */
@Service
public class RoleServiceImpl extends ServiceImpl<RoleMapper, Role> implements RoleService {

    /**
     * 部门角色
     */
    @Autowired
    private DepartmentRoleService departmentRoleService;
    /**
     * 用户权限
     */
    @Autowired
    private UserRoleService userRoleService;

    @Autowired
    private RoleMenuService roleMenuService;

    @Override
    public List<Role> findByDefaultRole(Boolean defaultRole) {
        QueryWrapper<Role> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("default_role", true);
        return baseMapper.selectList(queryWrapper);
    }

    @Override
    public void deleteRoles(List<String> roleIds) {
        QueryWrapper queryWrapper = new QueryWrapper<>();
        queryWrapper.in("role_id", roleIds);
        if (departmentRoleService.count(queryWrapper) > 0) {
            throw new ServiceException(ResultCode.PERMISSION_DEPARTMENT_ROLE_ERROR);
        }
        if (userRoleService.count(queryWrapper) > 0) {
            throw new ServiceException(ResultCode.PERMISSION_USER_ROLE_ERROR);
        }
        //删除角色
        this.removeByIds(roleIds);
        //删除角色与菜单关联
        roleMenuService.remove(queryWrapper);
    }
}
