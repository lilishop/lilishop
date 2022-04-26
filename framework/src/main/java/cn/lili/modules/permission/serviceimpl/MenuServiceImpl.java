package cn.lili.modules.permission.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.permission.entity.dos.Menu;
import cn.lili.modules.permission.entity.dos.RoleMenu;
import cn.lili.modules.permission.entity.dto.MenuSearchParams;
import cn.lili.modules.permission.entity.vo.MenuVO;
import cn.lili.modules.permission.mapper.MenuMapper;
import cn.lili.modules.permission.service.MenuService;
import cn.lili.modules.permission.service.RoleMenuService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.*;

/**
 * 权限业务层实现
 *
 * @author Chopper
 * @since 2020/11/17 3:49 下午
 */
@Slf4j
@Service
public class MenuServiceImpl extends ServiceImpl<MenuMapper, Menu> implements MenuService {
    /**
     * 菜单角色
     */
    @Autowired
    private RoleMenuService roleMenuService;

    @Autowired
    private Cache<List<Menu>> cache;

    @Override
    public void deleteIds(List<String> ids) {
        QueryWrapper<RoleMenu> queryWrapper = new QueryWrapper<>();
        queryWrapper.in("menu_id", ids);
        //如果已有角色绑定菜单，则不能直接删除
        if (roleMenuService.count(queryWrapper) > 0) {
            throw new ServiceException(ResultCode.PERMISSION_MENU_ROLE_ERROR);
        }
        this.removeByIds(ids);
    }


    @Override
    public List<MenuVO> findUserTree() {
        AuthUser authUser = Objects.requireNonNull(UserContext.getCurrentUser());
        if (Boolean.TRUE.equals(authUser.getIsSuper())) {
            return this.tree();
        }
        List<Menu> userMenus = this.findUserList(authUser.getId());
        return this.tree(userMenus);
    }

    @Override
    public List<Menu> findUserList(String userId) {
        String cacheKey = CachePrefix.USER_MENU.getPrefix(UserEnums.MANAGER) + userId;
        List<Menu> menuList = cache.get(cacheKey);
        if (menuList == null) {
            menuList = this.baseMapper.findByUserId(userId);
            //每5分钟重新确认用户权限
            cache.put(cacheKey, menuList, 300L);
        }
        return menuList;
    }

    /**
     * 添加更新菜单
     *
     * @param menu 菜单数据
     * @return 是否成功
     */
    @Override
    public boolean saveOrUpdateMenu(Menu menu) {
        if (CharSequenceUtil.isNotEmpty(menu.getId())) {

        }
        return this.saveOrUpdate(menu);
    }

    @Override
    public List<Menu> findByRoleIds(String roleId) {
        QueryWrapper<Menu> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("role_id", roleId);
        return this.list(queryWrapper);
    }

    @Override
    public List<Menu> searchList(MenuSearchParams menuSearchParams) {
        //title 需要特殊处理
        String title = null;
        if (CharSequenceUtil.isNotEmpty(menuSearchParams.getTitle())) {
            title = menuSearchParams.getTitle();
            menuSearchParams.setTitle(null);
        }
        QueryWrapper<Menu> queryWrapper = PageUtil.initWrapper(menuSearchParams, new SearchVO());
        if (CharSequenceUtil.isNotEmpty(title)) {
            queryWrapper.like("title", title);
        }
        queryWrapper.orderByDesc("sort_order");
        return this.baseMapper.selectList(queryWrapper);
    }


    @Override
    public List<MenuVO> tree() {
        try {
            List<Menu> menus = this.list();
            return tree(menus);
        } catch (Exception e) {
            log.error("菜单树错误", e);
        }
        return Collections.emptyList();
    }

    /**
     * 传入自定义菜单集合
     *
     * @param menus 自定义菜单集合
     * @return 修改后的自定义菜单集合
     */
    private List<MenuVO> tree(List<Menu> menus) {
        List<MenuVO> tree = new ArrayList<>();
        menus.forEach(item -> {
            if (item.getLevel() == 0) {
                MenuVO treeItem = new MenuVO(item);
                initChild(treeItem, menus);
                tree.add(treeItem);
            }
        });
        //对一级菜单排序
        tree.sort(Comparator.comparing(Menu::getSortOrder));
        return tree;
    }

    /**
     * 递归初始化子树
     *
     * @param tree  树结构
     * @param menus 数据库对象集合
     */
    private void initChild(MenuVO tree, List<Menu> menus) {
        if (menus == null) {
            return;
        }
        menus.stream()
                .filter(item -> (item.getParentId().equals(tree.getId())))
                .forEach(child -> {
                    MenuVO childTree = new MenuVO(child);
                    initChild(childTree, menus);
                    tree.getChildren().add(childTree);
                });
    }

}
