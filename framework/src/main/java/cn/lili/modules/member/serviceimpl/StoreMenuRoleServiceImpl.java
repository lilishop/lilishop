package cn.lili.modules.member.serviceimpl;

import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.member.entity.dos.StoreMenuRole;
import cn.lili.modules.member.entity.vo.StoreUserMenuVO;
import cn.lili.modules.member.mapper.StoreMenuRoleMapper;
import cn.lili.modules.member.mapper.StoreMenuMapper;
import cn.lili.modules.member.service.StoreMenuRoleService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * 角色菜单业务层实现
 * 员工菜单投影通过菜单存储查询，避免反向依赖菜单管理服务；角色维护仍保留原事务和缓存规则。
 *
 * @author Chopper
 * @since 2020/11/22 11:43
 */
@Slf4j
@Service
@Transactional(rollbackFor = Exception.class)
public class StoreMenuRoleServiceImpl extends ServiceImpl<StoreMenuRoleMapper, StoreMenuRole> implements StoreMenuRoleService {

    @Autowired
    private StoreMenuMapper storeMenuMapper;
    @Autowired
    private Cache<Object> cache;

    @Override
    public List<StoreMenuRole> findByRoleId(String roleId) {
        LambdaQueryWrapper<StoreMenuRole> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(StoreMenuRole::getRoleId, roleId);
        return this.baseMapper.selectList(queryWrapper);
    }

    /**
     * 读取员工角色菜单，沿用按会员主键缓存及空结果重新查询的规则。
     * 缓存未命中时执行原有菜单投影SQL，不改变角色、部门继承或超级权限查询条件。
     *
     * @param clerkId 店铺员工主键，用于菜单关联查询
     * @param memberId 会员主键，用于原有菜单缓存键
     * @return 员工可见的菜单权限列表
     */
    @Override
    public List<StoreUserMenuVO> findAllMenu(String clerkId, String memberId) {
        String cacheKey = CachePrefix.STORE_USER_MENU.getPrefix() + memberId;
        List<StoreUserMenuVO> menuList = (List<StoreUserMenuVO>) cache.get(cacheKey);
        if (menuList == null || menuList.isEmpty()) {
            menuList = storeMenuMapper.getUserRoleMenu(clerkId);
            cache.put(cacheKey, menuList);
        }
        return menuList;
    }


    @Override
    public void updateRoleMenu(String roleId, List<StoreMenuRole> roleMenus) {
        try {
            roleMenus.forEach(role -> role.setStoreId(UserContext.getCurrentUser().getStoreId()));
            //删除角色已经绑定的菜单
            this.delete(roleId);
            //重新保存角色菜单关系
            this.saveBatch(roleMenus);

            cache.vagueDel(CachePrefix.PERMISSION_LIST.getPrefix(UserEnums.STORE));
            cache.vagueDel(CachePrefix.STORE_USER_MENU.getPrefix());
        } catch (Exception e) {
            log.error("修改用户权限错误", e);
        }
    }

    @Override
    public void delete(String roleId) {
        //删除
        QueryWrapper<StoreMenuRole> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("role_id", roleId);
        this.remove(queryWrapper);
        cache.vagueDel(CachePrefix.PERMISSION_LIST.getPrefix(UserEnums.STORE));
        cache.vagueDel(CachePrefix.STORE_USER_MENU.getPrefix());
    }

    @Override
    public void delete(List<String> roleId) {
        //删除
        QueryWrapper<StoreMenuRole> queryWrapper = new QueryWrapper<>();
        queryWrapper.in("role_id", roleId);
        this.remove(queryWrapper);
        cache.vagueDel(CachePrefix.PERMISSION_LIST.getPrefix(UserEnums.STORE));
        cache.vagueDel(CachePrefix.STORE_USER_MENU.getPrefix());
    }
}
