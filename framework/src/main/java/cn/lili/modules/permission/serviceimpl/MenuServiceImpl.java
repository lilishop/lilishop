package cn.lili.modules.permission.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.PageUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.permission.entity.dos.Menu;
import cn.lili.modules.permission.entity.dos.RoleMenu;
import cn.lili.modules.permission.entity.dto.MenuSearchParams;
import cn.lili.modules.permission.entity.vo.MenuVO;
import cn.lili.modules.permission.mapper.MenuMapper;
import cn.lili.modules.permission.service.MenuService;
import cn.lili.modules.permission.service.RoleMenuService;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

/**
 * 权限业务层实现
 *
 * @author Chopper
 * @date 2020/11/17 3:49 下午
 */
@Slf4j
@Service
public class MenuServiceImpl extends ServiceImpl<MenuMapper, Menu> implements MenuService {
    /**
     * 菜单角色
     */
    @Autowired
    private RoleMenuService roleMenuService;

    @Override
    public void deleteIds(List<String> ids) {
        QueryWrapper<RoleMenu> queryWrapper = new QueryWrapper<>();
        queryWrapper.in("menu_id", ids);
        //如果已有角色绑定菜单，则不能直接删除
        if (roleMenuService.count(queryWrapper) > 0) {
            throw new ServiceException(ResultCode.PERMISSION_MENU_ROLE_ERROR);
        }
        this.removeByIds(ids);
        //删除关联关系
        roleMenuService.deleteRoleMenu(ids);
    }


    @Override
    public List<MenuVO> findUserTree() {
        AuthUser authUser = UserContext.getCurrentUser();
        if (authUser.getIsSuper()) {
            return this.tree();
        }
        List<Menu> userMenus = this.baseMapper.findByUserId(authUser.getId());
        return this.tree(userMenus);
    }

    @Override
    public List<Menu> findUserList(String userId) {
        return this.baseMapper.findByUserId(userId);
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
        if (StringUtils.isNotEmpty(menuSearchParams.getTitle())) {
            title = menuSearchParams.getTitle();
            menuSearchParams.setTitle(null);
        }
        QueryWrapper queryWrapper = PageUtil.initWrapper(menuSearchParams, new SearchVO());
        if (StringUtils.isNotEmpty(title)) {
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
        return null;
    }

    /**
     * 传入自定义菜单集合
     *
     * @param menus
     * @return
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
        tree.sort(new Comparator<MenuVO>() {
            @Override
            public int compare(MenuVO o1, MenuVO o2) {
                return o1.getSortOrder().compareTo(o2.getSortOrder());
            }
        });
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
