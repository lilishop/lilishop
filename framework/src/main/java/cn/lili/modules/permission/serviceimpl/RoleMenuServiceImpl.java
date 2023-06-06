package cn.lili.modules.permission.serviceimpl;

import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.permission.entity.dos.RoleMenu;
import cn.lili.modules.permission.mapper.RoleMenuMapper;
import cn.lili.modules.permission.service.RoleMenuService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import groovy.util.logging.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * 角色菜单业务层实现
 *
 * @author Chopper
 * @since 2020/11/22 11:43
 */
@Slf4j
@Service
public class RoleMenuServiceImpl extends ServiceImpl<RoleMenuMapper, RoleMenu> implements RoleMenuService {

    @Autowired
    private Cache<Object> cache;

    @Override
    public List<RoleMenu> findByRoleId(String roleId) {
        LambdaQueryWrapper<RoleMenu> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(RoleMenu::getRoleId, roleId);
        return this.baseMapper.selectList(queryWrapper);
    }


    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateRoleMenu(String roleId, List<RoleMenu> roleMenus) {
        try {
            //删除角色已经绑定的菜单
            this.deleteRoleMenu(roleId);
            //重新保存角色菜单关系
            this.saveBatch(roleMenus);

            cache.vagueDel(CachePrefix.USER_MENU.getPrefix(UserEnums.MANAGER));
            cache.vagueDel(CachePrefix.PERMISSION_LIST.getPrefix(UserEnums.MANAGER));
        } catch (Exception e) {
            log.error("修改用户权限错误", e);
        }
    }

    @Override
    public void deleteRoleMenu(String roleId) {
        //删除
        QueryWrapper<RoleMenu> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("role_id", roleId);
        cache.vagueDel(CachePrefix.USER_MENU.getPrefix(UserEnums.MANAGER));
        cache.vagueDel(CachePrefix.PERMISSION_LIST.getPrefix(UserEnums.MANAGER));
        this.remove(queryWrapper);
        
    }

    @Override
    public void deleteRoleMenu(List<String> roleId) {
        //删除
        QueryWrapper<RoleMenu> queryWrapper = new QueryWrapper<>();
        queryWrapper.in("role_id", roleId);
        cache.vagueDel(CachePrefix.USER_MENU.getPrefix(UserEnums.MANAGER));
        cache.vagueDel(CachePrefix.PERMISSION_LIST.getPrefix(UserEnums.MANAGER));
        this.remove(queryWrapper);
        
    }
}