package cn.lili.modules.member.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.modules.member.entity.dos.StoreRole;
import cn.lili.modules.member.mapper.StoreRoleMapper;
import cn.lili.modules.member.service.StoreClerkRoleService;
import cn.lili.modules.member.service.StoreDepartmentRoleService;
import cn.lili.modules.member.service.StoreMenuRoleService;
import cn.lili.modules.member.service.StoreRoleService;
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
@Transactional(rollbackFor = Exception.class)
public class StoreRoleServiceImpl extends ServiceImpl<StoreRoleMapper, StoreRole> implements StoreRoleService {

    /**
     * 部门角色
     */
    @Autowired
    private StoreDepartmentRoleService storeDepartmentRoleService;
    /**
     * 用户权限
     */
    @Autowired
    private StoreClerkRoleService storeClerkRoleService;

    @Autowired
    private StoreMenuRoleService storeMenuRoleService;

    @Override
    public List<StoreRole> findByDefaultRole(Boolean defaultRole) {
        QueryWrapper<StoreRole> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("default_role", true);
        return baseMapper.selectList(queryWrapper);
    }

    @Override
    public void deleteRoles(List<String> roleIds) {
        //校验是否为当前店铺
        QueryWrapper queryWrapper = new QueryWrapper<>();
        queryWrapper.in("id", roleIds);
        List<StoreRole> roles = this.baseMapper.selectList(queryWrapper);
        roles.forEach(role -> {
            if (!role.getStoreId().equals(UserContext.getCurrentUser().getStoreId())) {
                throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
            }
        });
        queryWrapper = new QueryWrapper<>();
        queryWrapper.in("role_id", roleIds);
        //校验是否绑定店铺部门
        if (storeDepartmentRoleService.count(queryWrapper) > 0) {
            throw new ServiceException(ResultCode.PERMISSION_DEPARTMENT_ROLE_ERROR);
        }
        //校验是否绑定店员
        if (storeClerkRoleService.count(queryWrapper) > 0) {
            throw new ServiceException(ResultCode.PERMISSION_USER_ROLE_ERROR);
        }
        //删除角色
        this.removeByIds(roleIds);
        //删除角色与菜单关联
        storeMenuRoleService.remove(queryWrapper);
    }

    @Override
    public Boolean update(StoreRole storeRole) {
        StoreRole storeRoleTemp = this.getById(storeRole.getId());
        //校验店铺角色是否存在
        if (storeRoleTemp == null) {
            throw new ServiceException(ResultCode.PERMISSION_ROLE_NOT_FOUND_ERROR);
        }
        //校验店铺角色是否属于当前店铺
        if (!storeRoleTemp.getStoreId().equals(UserContext.getCurrentUser().getStoreId())) {
            throw new ServiceException(ResultCode.PERMISSION_ROLE_NOT_FOUND_ERROR);
        }
        return updateById(storeRole);
    }


    @Override
    public Boolean saveStoreRole(StoreRole storeRole) {
        storeRole.setStoreId(UserContext.getCurrentUser().getStoreId());
        return save(storeRole);
    }

    @Override
    public List<StoreRole> list(List<String> ids) {
        QueryWrapper<StoreRole> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("store_id", UserContext.getCurrentUser().getStoreId());
        queryWrapper.in("id", ids);
        return this.baseMapper.selectList(queryWrapper);
    }
}
